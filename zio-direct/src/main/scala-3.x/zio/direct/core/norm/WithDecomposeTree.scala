package zio.direct.core.norm

import scala.quoted._
import zio.direct.core.metaprog.Extractors._
import zio.direct.core.metaprog._
import zio.direct._
import zio.direct.core.util.Format
import scala.collection.mutable
import zio.Chunk
import zio.direct.core.util.PureTree
import zio.direct.core.metaprog.WithPrintIR
import zio.direct.core.metaprog.Embedder._
import zio.direct.core.norm.WithComputeType
import zio.direct.core.norm.WithReconstructTree
import zio.direct.core.util.Unsupported
import zio.direct.core.util.ShowDetails
import zio.direct.Internal.deferred
import zio.direct.Internal.ignore
import zio.direct.core.util.WithInterpolator
import zio.direct.core.util.Messages

trait WithDecomposeTree {
  self: WithF with WithPrintIR with WithIR with WithZioType =>

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  protected object Decompose:
    def apply[F[_, _, _]: Type, S: Type, W: Type](monad: DirectMonad[F, S, W], effectType: ZioEffectType, instr: Instructions) =
      new Decompose(monad, effectType, instr)

  protected class Decompose[F[_, _, _]: Type, S: Type, W: Type](monad: DirectMonad[F, S, W], et: ZioEffectType, instr: Instructions):
    def apply(value: Term) = DecomposeTree.orPure(value)
    // object RunCall extends RunCallExtractor[F]

    private def isRunCallEffectOrFail(runCall: Term): Unit =
      if (!et.isEffectOf(runCall.tpe))
        report.errorAndAbort(s"The effect-type of the of the below run call was `${runCall.tpe.widenTermRefByName.show}` but this defer-block expects a `${et.tpe.widenTermRefByName.show}`:\n${Format.Term(runCall)}")

    private object DecomposeTree {
      def orPure(term: Term): IR =
        unapply(term).getOrElse(IR.Pure(term))

      def orPure2(termA: Term, termB: Term): (IR, IR) =
        (unapply(termA).getOrElse(IR.Pure(termA)), unapply(termB).getOrElse(IR.Pure(termB)))

      def unapply(expr: Term): Option[IR.Monadic] = {
        val ret = expr match {

          // if there is a nested defer call, the inner tree should already be pure
          // but as an optimization we can just roll up the thing into an IR.Monad
          // because we know it doesn't need to be transformed anymore
          case Seal('{ deferred($effect) }) =>
            Some(IR.Monad('{ ${ monad.Success }.unit($effect) }.asTerm, IR.Monad.Source.PrevDefer))

          // Since ValDef needs to be decomposed even if it's not a tree, need to do this before PureTree
          case block @ Block(parts, lastPart) =>
            DecomposeBlock.unapply(block)

          // Otherwise, if there are no Run calls treat the tree as "Pure" i.e.
          // it will be either embedded within the parent map/flatMap clause or
          // wrapped into a ZIO.succeed.
          case PureTree(tree) =>
            None

          case DecomposeSingleTermConstruct(monad) => Some(monad)

          case AnyRunCall(task) =>
            isRunCallEffectOrFail(task.asTerm)
            Some(IR.Monad(task.asTerm))

          case Typed(tree, _) =>
            unapply(tree)

          // This is perhaps the most powerful component of the monadless-paradigm. It changes something that looks like this:
          // { (unlift(foo), unlift(bar)) }
          // To something that looks like:
          // { ZIO.collect(foo, bar).map(iter => val a = iter.next(); val b = iter.next(); (a, b)) }
          // Since this introduces all the correctness issues that disallow mutable variables we
          // allow a mode that specifically disallows it
          case term =>
            if (instr.linearity == Linearity.Linear)
              Unsupported.Error.withTree(term, Messages.UnsafeNotAllowedParallel, InfoBehavior.Info)

            val unlifts = mutable.ArrayBuffer.empty[(IR.Monadic, Symbol)]
            val newTree: Term =
              Trees.Transform(term, Symbol.spliceOwner) {
                case originalTerm @ AnyRunCall(task) =>
                  val tpe = originalTerm.tpe
                  val sym = Symbol.newVal(Symbol.spliceOwner, "runVal", tpe, Flags.EmptyFlags, Symbol.noSymbol)
                  unlifts += ((IR.Monad(task.asTerm), sym))
                  Ref(sym)

                case originalTerm @ DecomposeSingleTermConstruct(monad) =>
                  val tpe = originalTerm.tpe
                  val sym = Symbol.newVal(Symbol.spliceOwner, "singleVal", tpe, Flags.EmptyFlags, Symbol.noSymbol)
                  unlifts += ((monad, sym))
                  Ref(sym)

                case originalTerm @ DecomposeBlock(monad) =>
                  // Take the type from the originalTerm (i.e. the result of the run call since it could be a block etc...)
                  val tpe = originalTerm.tpe
                  val sym = Symbol.newVal(Symbol.spliceOwner, "blockVal", tpe, Flags.EmptyFlags, Symbol.noSymbol)
                  unlifts += ((monad, sym))
                  Ref(sym)
              }
            Some(IR.Parallel(term, unlifts.toList, IR.Pure(newTree)))
        }
        ret
      }
    }

    /**
     * DecomposeTree a sequence of steps
     * a; b = unlift(zio); c
     * Into a.flatMap()
     */
    private object DecomposeBlock {
      def unapply(block: Block): Option[IR.Monadic] =
        val Block(head, tail) = block
        val parts = head :+ tail
        parts match {
          // This is the most important use-case of the monadless system.
          // Change this:
          //   val x = unlift(stuff)
          //   stuff-that-uses-x
          // Into this:
          //   stuff.flatMap(x => ...)
          //   stuff-that-uses-x
          //
          // This basically does that with some additional details
          // (e.g. it can actually be stuff.flatMap(v => val x = v; stuff-that-uses-x))
          // TODO A zero-args DefDef (i.e. a ByName can essentially be treated as a ValDef so we can use that too)

          // A ValDef statement is basically a block because it consists of
          //   {
          //   val x = run(monad)
          //   otherStuff
          //   }
          // The entire above block is represented by the valdef:
          //   IR.ValDef({val x...}, x, monad, otherStuff)
          case (origStmt @ ValDefStatement(symbol, assignmentTerm)) :: tail =>
            val assignment =
              assignmentTerm match {
                case DecomposeTree(monad) => monad
                case _                    => IR.Pure(assignmentTerm)
              }
            val restOfBlock: Block = BlockN(origStmt +: tail)
            val out =
              BlockN(tail) match
                case DecomposeTree(monadBody) =>
                  IR.ValDef(restOfBlock, symbol, assignment, monadBody)
                case pureBody =>
                  IR.ValDef(restOfBlock, symbol, assignment, IR.Pure(pureBody))
            Some(out)

          // If the rest of the structure is a pure tree then exit
          // TODO should be checking all of them, need example of treel with multiple statements
          case BlockN(PureTree(tree)) =>
            None

          case DecomposeTree(monad) :: tail =>
            tail match {
              // In this case where is one statement in the block which my definition
              // needs to have the same type as the output: e.g.
              //   val v: T = { unlift(doSomething:ZIO[_, _, T]) }
              // since we've pulled out the `doSomething` inside the signature
              // will be ZIO[_, _, T] instead of T.
              case Nil =>
                Some(monad)
              case list =>
                // In this case there are multiple instructions inside the seauence e.g:
                //   val v: T = { unlift(x), y /*pure*/, unlift(z:ZIO[_, _, T]) }
                // We recurse by flatMapping x, y, and unlift... but eventually the type
                // also has to be ZIO[_, _, T] since we are essentially doing:
                // x.flatMap(.. -> {y; z: ZIO[_, _, T]}). Of course the value will be ZIO[_, _, T]
                // since the last value of a nested flatMap chain is just the last instruction
                // in the nested sequence.
                val out =
                  BlockN(tail) match
                    case DecomposeTree(bodyMonad) =>
                      IR.FlatMap(monad, None, bodyMonad)
                    case bodyPure =>
                      IR.Map(monad, None, IR.Pure(bodyPure))
                Some(out)
            }

          // This is the recursive case of DecomposeBlock, it will work across multiple things
          // between blocks due to the recursion e.g:
          //   val blah = new Blah(2) // 1st part, will recurse 1st time (here)
          //   import blah._          // 2nd part, will recurse 2nd time (here)
          //   val b = unlift(ZIO.succeed(value).asInstanceOf[Task[Int]]) // Then will match valdef case
          case head :: BlockN(DecomposeBlock(parts)) =>
            Unsupported.Warn.checkUnmooredZio(et.isEffectOf)(head)
            Some(IR.Block(head, parts))

          case other =>
            None
        }
    }

    private object DecomposeSingleTermConstruct {
      def unapply(term: Tree) =
        term match {
          case Seal('{ unsafe($value) }) =>
            Some(IR.Unsafe(DecomposeTree.orPure(value.asTerm)))

          // If we have a special user-defined "ignore" block, just splice the code. The `ignore` construct
          // is should ONLY be used to test code.
          case Seal('{ ignore($code) }) =>
            if (et.isEffectOf(code.asTerm.tpe)) {
              Some(IR.Monad(code.asTerm, IR.Monad.Source.IgnoreCall))
            } else {
              Some(IR.Monad(monad.Value.succeed(code.asTerm).asTerm, IR.Monad.Source.IgnoreCall))
            }

          case DecomposeUtilityCall(utilityIR) => Some(utilityIR)

          case tryTerm @ Try(tryBlock, cases, finallyBlock) =>
            // Don't need the above terms
            // val tryBlockIR = DecomposeTree.orPure(tryBlock)
            // val cases = DecomposeCases(caseDefs)
            val casesOptIRTs =
              if (cases.nonEmpty) {
                Some(DecomposeCases(cases))
              } else {
                None
              }
            val caseDefsOpt = casesOptIRTs.map(IR.Match.CaseDefs(_))

            val tryDecomposed = DecomposeTree.orPure(tryBlock)
            // println(s"--------- Try Type: ${PrintIR(tryDecomposed)}")

            Some(IR.Try(tryDecomposed, caseDefsOpt, tryTerm.tpe, finallyBlock.map(DecomposeTree.orPure(_))))

          case Seal('{ throw $e }) =>
            Some(IR.Fail(DecomposeTree.orPure(e.asTerm)))

          case If(cond, ifTrue, ifFalse) =>
            // NOTE: Code below is quite inefficient, do instead do this:
            // (DecomposeTree.unapply(ifTrue), DecomposeTree.unapply(ifFalse)). Then match on the Some/Some, Some/None, etc... cases
            val (ifTrueIR, ifFalseIR) = DecomposeTree.orPure2(ifTrue, ifFalse)
            val condIR = DecomposeTree.orPure(cond)
            Some(IR.If(condIR, ifTrueIR, ifFalseIR))

          // For example, this:
          //   while (foo) { run(bar:ZIO) }
          // will be translated into:
          //   def whileFunc() { if (foo) { bar; whileFunc() } }
          // the actual function will be synthesized in the Reconstructor. Ultimately the following code will be spliced:
          //   { def whileFunc() { if (foo) { bar; whileFunc() } } }
          //
          // in the case that `if` is a monad, i.e. something like this
          //   while (run(foo)) { run(bar) }
          // then something like this will happen:
          //   def whileFunc() { foo.flatMap(fooVal => { if (foo) { bar; whileFunc() } })) }
          case While(cond, body) =>
            Some(IR.While(DecomposeTree.orPure(cond), DecomposeTree.orPure(body)))

          case Seal('{ ($list: Iterable[t]).foreach(${ Lambda1(sym, expr) }) }) =>
            val monad = DecomposeTree.orPure(list.asTerm)
            val body = DecomposeTree.orPure(expr.asTerm)
            Some(IR.Foreach(monad, list.asTerm.tpe, sym, body))

          case Seal('{ ($a: Boolean) && ($b: Boolean) }) =>
            // the actual case where they are both cure is handled by the PureTree case
            val (aTerm, bTerm) = DecomposeTree.orPure2(a.asTerm, b.asTerm)
            Some(IR.And(aTerm, bTerm))

          case Seal('{ ($a: Boolean) || ($b: Boolean) }) =>
            // the actual case where they are both cure is handled by the PureTree case
            val (aTerm, bTerm) = DecomposeTree.orPure2(a.asTerm, b.asTerm)
            Some(IR.Or(aTerm, bTerm))

          case Match(m @ DecomposeTree(monad), caseDefs) =>
            // Since in Scala 3 we cannot just create a arbitrary symbol and pass it around.
            // (See https://github.com/lampepfl/dotty/blob/33818506801c80c8c73649fdaab3782c052580c6/library/src/scala/quoted/Quotes.scala#L3675)
            // In order to be able to have a valdef-symbol to manipulate, we need to create the actual valdef
            // Therefore we need to create
            // a synthetic val-def for the symbol with which to substitute this expression.
            // For example if we want to substitute something like this:
            //   unlift(ZIO.attempt(stuff)) match { case ...can-use-m.... }
            // We need to do something like this:
            //   ZIO.attempt(stuff).map(m => match { case ...can-use-m.... })
            // However, in order to be able to get the `m => ...`
            // We first need to create a fake val-def + right-hand-side that looks like:
            //   '{ val m:StuffType = ???; ...can-use-m.... }
            // So the Nest-call can actually change it to:
            //   ZIO.attempt(stuff).map(m => match { case ...can-use-m.... })
            val (oldSymbol, body) =
              useNewSymbolIn(m.tpe)(sym => Match(sym, caseDefs))

            val out =
              body match
                case DecomposeTree(bodyMonad) =>
                  IR.FlatMap(monad, oldSymbol, bodyMonad)
                case bodyPure =>
                  IR.Map(monad, oldSymbol, IR.Pure(bodyPure))
            Some(out)

          case m @ Match(value, DecomposeCases(cases)) =>
            val caseDefs =
              if (cases.nonEmpty) {
                IR.Match.CaseDefs(cases)
              } else {
                report.errorAndAbort(s"Invalid match statement with no cases:\n${Format.Term(m)}")
              }
            Some(IR.Match(IR.Pure(value), caseDefs))

          case _ => None
        }
    }

    private object DecomposeCases {
      def apply(cases: List[CaseDef]): List[IR.Match.CaseDef] =
        applyMark(cases)

      private def applyMark(cases: List[CaseDef]) =
        cases.map {
          case CaseDef(pattern, cond, DecomposeTree(body)) =>
            IR.Match.CaseDef(pattern, cond, body)
          case CaseDef(pattern, cond, body) =>
            IR.Match.CaseDef(pattern, cond, IR.Monad(monad.Value.succeed(body).asTerm))
        }

      def unapply(cases: List[CaseDef]) =
        // If at least one of the match-cases need to be transformed, transform all of them
        Some(applyMark(cases))
    }

    private object DecomposeUtilityCall {
      implicit val implicitInsr: Instructions = instr
      private def typicalError(effectName: String, term: Tree) =
        Unsupported.Error.withTree(
          term,
          s"""|Cannot call a ${effectName}-effect on the effect-type ${Format.TypeRepr(et.tpe)}.
              |There is no instance of a MonadState defined for it.
              |""".stripMargin
        )

      def unapply(term: Tree) =
        term match {
          case term @ AnyGetCall() =>
            // NOTE interesting that getting SBase is not needed here
            monad.MonadState match
              case Some(stateMonad) => Some(IR.Monad('{ $stateMonad.get }.asTerm))
              case None             => typicalError("State get", term)

          case term @ AnySetCall(setValue) =>
            monad.MonadState match
              case Some(stateMonad) =>
                Some(IR.Monad('{ ${ stateMonad.asExprOf[MonadState[F, S]] }.set(${ setValue.asExprOf[S] }) }.asTerm))

              // Note that if this whole approach with casting to MonadState[F, ...] doesn't work,
              // we could always try to cast stateMonad to MonadState[F, Any] or some other value
              // and also cast setValue as that same Any (or some other value). However this is
              // a less type-full solution so it should only be used in last-resort.
              // setValue.asTerm.tpe.asType match
              //   case '[l] =>
              //     Some(IR.Monad('{ ${ stateMonad.asExprOf[MonadState[F]] }.set(${ setValue.asExprOf[l] }) }.asTerm))
              case None =>
                typicalError("State set", term)

          case term @ AnyLogCall(logValue) =>
            monad.MonadLog match
              case Some(logMonad) =>
                Some(IR.Monad('{ ${ logMonad.asExprOf[MonadLog[F, W]] }.log(${ logValue.asExprOf[W] }) }.asTerm))
              case None =>
                typicalError("logging", term)
          case _ =>
            None
        }
    }

  end Decompose
}
