package zio.direct.core.norm

import scala.quoted._
import zio.Task
import zio.direct.core.metaprog.Extractors._
import zio.direct.core.metaprog._
import zio.direct._
import zio.direct.core.util.Format
import zio.ZIO
import scala.collection.mutable
import zio.Chunk
import zio.direct.core.util.PureTree
import zio.direct.core.util.ComputeTotalZioType
import zio.direct.core.metaprog.WithPrintIR
import zio.direct.core.metaprog.Embedder._
import zio.direct.core.norm.WithComputeType
import zio.direct.core.norm.WithReconstructTree
import zio.direct.core.util.ShowDetails

trait WithDecomposeTree {
  self: WithIR =>

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  protected object DecomposeTree {
    def orPure(term: Term): IR =
      unapply(term).getOrElse(IR.Pure(term))

    def orPure2(termA: Term, termB: Term): (IR, IR) =
      (unapply(termA).getOrElse(IR.Pure(termA)), unapply(termB).getOrElse(IR.Pure(termB)))

    def unapply(expr: Term): Option[IR.Monadic] = {
      val ret = expr match {

        // Certain constructs should always be translated into ZIO constructs
        // even if they do not contain any Await calls. These involve error handling.
        case Seal('{ unsafe($value) }) =>
          Some(IR.Unsafe(DecomposeTree.orPure(value.asTerm)))

        case tryTerm @ Try(tryBlock, caseDefs, finallyBlock) =>
          val tryBlockIR = DecomposeTree.orPure(tryBlock)
          val cases = DecomposeCases(caseDefs)
          Some(IR.Try(DecomposeTree.orPure(tryBlock), DecomposeCases(caseDefs), tryTerm.tpe, finallyBlock.map(DecomposeTree.orPure(_))))

        case Seal('{ throw $e }) =>
          Some(IR.Fail(DecomposeTree.orPure(e.asTerm)))

        // Otherwise, if there are no Await calls treat the tree as "Pure" i.e.
        // it will be either embedded within the parent map/flatMap clause or
        // wrapped into a ZIO.succeed.
        case PureTree(tree) =>
          None

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

        case Seal('{ ($a: Boolean) && ($b: Boolean) }) =>
          // the actual case where they are both cure is handled by the PureTree case
          val (aTerm, bTerm) = DecomposeTree.orPure2(a.asTerm, b.asTerm)
          Some(IR.And(aTerm, bTerm))

        case Seal('{ ($a: Boolean) || ($b: Boolean) }) =>
          // the actual case where they are both cure is handled by the PureTree case
          val (aTerm, bTerm) = DecomposeTree.orPure2(a.asTerm, b.asTerm)
          Some(IR.Or(aTerm, bTerm))

        case block @ Block(parts, lastPart) =>
          DecomposeBlock.unapply(block)

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
          Some(IR.Match(IR.Pure(value), cases))

        case RunCall(task) =>
          Some(IR.Monad(task.asTerm))

        case Typed(tree, _) =>
          unapply(tree)

        // This is perhaps the most powerful component of the monadless-paradigm. It changes something that looks like this:
        // { (unlift(foo), unlift(bar)) }
        // To something that looks like:
        // { ZIO.collect(foo, bar).map(iter => val a = iter.next(); val b = iter.next(); (a, b)) }
        case term => // @ Allowed.ParallelExpression()
          // println(s"========== GENERIC CONSTRUCT: ${Format.Tree(term)}\n==============\n${Format(Printer.TreeStructure.show(term))}")

          val unlifts = mutable.ArrayBuffer.empty[(IR.Monadic, Symbol)]
          val newTree: Term =
            Trees.Transform(term, Symbol.spliceOwner) {
              case originalTerm @ RunCall(task) =>
                val tpe = originalTerm.tpe
                val sym = Symbol.newVal(Symbol.spliceOwner, "par", tpe, Flags.EmptyFlags, Symbol.noSymbol)
                unlifts += ((IR.Monad(task.asTerm), sym))
                Ref(sym)

              case originalTerm @ DecomposeBlock(monad) =>
                // Take the type from the originalTerm (i.e. the result of the await call since it could be a block etc...)
                val tpe = originalTerm.tpe
                val sym = Symbol.newVal(Symbol.spliceOwner, "par", tpe, Flags.EmptyFlags, Symbol.noSymbol)
                unlifts += ((monad, sym))
                Ref(sym)
            }
          Some(IR.Parallel(unlifts.toList, IR.Pure(newTree)))
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

        case ValDefStatement(symbol, DecomposeTree(monad)) :: tail =>
          val out =
            BlockN(tail) match
              case DecomposeTree(monadBody) =>
                IR.FlatMap(monad, symbol, monadBody)
              case pureBody =>
                IR.Map(monad, symbol, IR.Pure(pureBody))
          Some(out)

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
          head match
            case term: Term =>
              term.tpe.asType match
                case '[ZIO[r, e, a]] if (!(term.tpe =:= TypeRepr.of[Nothing])) =>
                  report.warning(
                    s"Found a ZIO term that is not being awaited (type: ${Format.TypeRepr(term.tpe)}). Non-awaited ZIO terms inside of `{ ... }` blocks will never be executed i.e. they will be discarded. " +
                      s"To execute this term add `.run` at the end or wrap it into an `run(...)` statement." +
                      s"\n========\n" +
                      Format.Term(term),
                    term.asExpr
                  )
                case _ =>
            case _ =>

          Some(IR.Block(head, parts))

        case other =>
          None
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
          IR.Match.CaseDef(pattern, cond, IR.Monad(ZioApply.succeed(body).asTerm))
      }

    def unapply(cases: List[CaseDef]) =
      // If at least one of the match-cases need to be transformed, transform all of them
      Some(applyMark(cases))
  }
}
