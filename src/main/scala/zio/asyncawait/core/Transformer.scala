package zio.asyncawait.core

import scala.quoted._
import zio.Task
import zio.asyncawait.core.metaprog.Extractors._
import zio.asyncawait.core.metaprog._
import zio.asyncawait._
import zio.asyncawait.core.util.Format
import zio.ZIO
import scala.collection.mutable
import zio.Chunk
import zio.asyncawait.core.util.PureTree
import zio.asyncawait.core.util.ComputeTotalZioType
import zio.asyncawait.core.metaprog.ModelPrinting
import zio.asyncawait.core.metaprog.Embedder._
import zio.asyncawait.core.norm.ModelTypeComputation
import zio.asyncawait.core.norm.ModelReconstructor

// TODO replace all instances of ZIO.succeed with ZIO.attempt?
//      need to look through cases to see which ones expect errors
class Transformer(inputQuotes: Quotes)
  extends Model
  with ModelTypeComputation
  with ModelPrinting
  with ModelReconstructor {

  implicit val macroQuotes = inputQuotes
  import quotes.reflect._

  private object Decompose {
    def orPure(term: Term): IR =
      unapply(term).getOrElse(IR.Pure(term))

    def orPure2(termA: Term, termB: Term): (IR, IR) =
      (unapply(termA).getOrElse(IR.Pure(termA)), unapply(termB).getOrElse(IR.Pure(termB)))

    def unapply(expr: Term): Option[IR.Monadic] = {
      val ret = expr match {
        case PureTree(tree) =>
          None

        case If(cond, ifTrue, ifFalse) =>
          // NOTE: Code below is quite inefficient, do instead do this:
          // (Decompose.unapply(ifTrue), Decompose.unapply(ifFalse)). Then match on the Some/Some, Some/None, etc... cases
          val (ifTrueIR, ifFalseIR) = Decompose.orPure2(ifTrue, ifFalse)
          val condIR = Decompose.orPure(cond)
          Some(IR.If(condIR, ifTrueIR, ifFalseIR))

        // For example, this:
        //   while (foo) { await(bar:ZIO) }
        // will be translated into:
        //   def whileFunc() { if (foo) { bar; whileFunc() } }
        // the actual function will be synthesized in the Reconstructor. Ultimately the following code will be spliced:
        //   { def whileFunc() { if (foo) { bar; whileFunc() } } }
        //
        // in the case that `if` is a monad, i.e. something like this
        //   while (await(foo)) { await(bar) }
        // then something like this will happen:
        //   def whileFunc() { foo.flatMap(fooVal => { if (foo) { bar; whileFunc() } })) }
        case While(cond, body) =>
          Some(IR.While(Decompose.orPure(cond), Decompose.orPure(body)))

        case Seal('{ unsafe($value) }) =>
          Some(IR.Unsafe(Decompose.orPure(value.asTerm)))

        case Seal('{ ($a: Boolean) && ($b: Boolean) }) =>
          // the actual case where they are both cure is handled by the PureTree case
          val (aTerm, bTerm) = Decompose.orPure2(a.asTerm, b.asTerm)
          Some(IR.And(aTerm, bTerm))

        case Seal('{ ($a: Boolean) || ($b: Boolean) }) =>
          // the actual case where they are both cure is handled by the PureTree case
          val (aTerm, bTerm) = Decompose.orPure2(a.asTerm, b.asTerm)
          Some(IR.Or(aTerm, bTerm))

        case tryTerm @ Try(tryBlock, caseDefs, finallyBlock) =>
          val tryBlockIR = Decompose.orPure(tryBlock)
          val cases = DecomposeCases(caseDefs)
          Some(IR.Try(Decompose.orPure(tryBlock), DecomposeCases(caseDefs), tryTerm.tpe, finallyBlock.map(Decompose.orPure(_))))

        case block @ Block(parts, lastPart) =>
          DecomposeBlock.unapply(block)

        case Match(m @ Decompose(monad), caseDefs) =>
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
              case Decompose(bodyMonad) =>
                IR.FlatMap(monad, oldSymbol, bodyMonad)
              case bodyPure =>
                IR.Map(monad, oldSymbol, IR.Pure(bodyPure))
          Some(out)

        case m @ Match(value, DecomposeCases(cases)) =>
          Some(IR.Match(IR.Pure(value), cases))

        case Seal('{ await[r, e, a]($task) }) =>
          Some(IR.Monad(task.asTerm))

        case Typed(tree, _) =>
          unapply(tree)


        // This is perhaps the most powerful component of the monadless-paradigm. It changes something that looks like this:
        // { (unlift(foo), unlift(bar)) }
        // To something that looks like:
        // { ZIO.collect(foo, bar).map(iter => val a = iter.next(); val b = iter.next(); (a, b)) }
        case term => // @ Allowed.ParallelExpression()
          //println(s"========== GENERIC CONSTRUCT: ${Format.Tree(term)}\n==============\n${Format(Printer.TreeStructure.show(term))}")

          val unlifts = mutable.ArrayBuffer.empty[(IR.Monadic, Symbol)]
          val newTree: Term =
            Trees.Transform(term, Symbol.spliceOwner) {
              case originalTerm @ Seal('{ await[r, e, a]($task) }) =>
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
    * Decompose a sequence of steps
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

        case ValDefStatement(symbol , Decompose(monad)) :: tail =>
          val out =
            BlockN(tail) match
              case Decompose(monadBody) =>
                IR.FlatMap(monad, symbol, monadBody)
              case pureBody =>
                IR.Map(monad, symbol, IR.Pure(pureBody))
          Some(out)

        case Decompose(monad) :: tail =>
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
                  case Decompose(bodyMonad) =>
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
                case '[ZIO[r, e, a]] =>
                  report.warning(
                    s"Found a ZIO term that is not being awaited. Non-awaited ZIO terms inside of `{ ... }` blocks will never be executed i.e. they will be discarded. " +
                    s"To execute this term add `.run` at the end or wrap it into an `await(...)` statement." +
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
        case CaseDef(pattern, cond, Decompose(body)) =>
          IR.Match.CaseDef(pattern, cond, body)
        case CaseDef(pattern, cond, body) =>
          IR.Match.CaseDef(pattern, cond, IR.Monad(ZioApply(body).asTerm))
      }

    def unapply(cases: List[CaseDef]) =
      // If at least one of the match-cases need to be transformed, transform all of them
      Some(applyMark(cases))
  }

  def symbolLineage(sym: Symbol): Unit =
    if (sym.isNoSymbol) {
      ()
    } else {
      symbolLineage(sym.owner)
    }

  def apply[T: Type](valueRaw: Expr[T], instructions: Instructions): Expr[ZIO[?, ?, ?]] = {
    val value = valueRaw.asTerm.underlyingArgument

    // // Do a top-level transform to check that there are no invalid constructs
    Allowed.validateBlocksIn(value.asExpr, instructions)
    // // Do the main transformation
    val transformedRaw = Decompose.orPure(value)

    if (instructions.info.showDeconstructed)
      println("============== Deconstructed Instructions ==============\n" + mprint(transformedRaw))

    val transformed = WrapUnsafes(transformedRaw)
    val transformedSameAsRaw = transformed != transformedRaw
    if (instructions.info.showDeconstructed) {
      if (transformedSameAsRaw)
        println("============== Monadified Tries ==============\n" + mprint(transformed))
      else
        println("============== Monadified Tries (No Changes) ==============")
    }


    val output = new Reconstruct(instructions)(transformed)
    if (instructions.info.showReconstructed)
      println("============== Reconstituted Code ==============\n" + Format.Expr(output))

    if (instructions.info.showReconstructedTree)
      println("============== Reconstituted Code Raw ==============\n" + Format(Printer.TreeStructure.show(output.asTerm)))

    val computedType = ComputeType(transformed)

    val zioType = computedType.toZioType

    if (instructions.info.showReconstructed)
      println(
        s"""-------------
        |Computed-Type: ${Format.TypeRepr(zioType)}
        |Discovered-Type: ${Format.TypeRepr(output.asTerm.tpe)}
        |Is Subtype: ${zioType <:< output.asTerm.tpe}
        |""".stripMargin
      )

    // // TODO verify that there are no await calls left. Otherwise throw an error
    val ownerPositionOpt = topLevelOwner.pos

    (computedType.r.asType, computedType.e.asType, computedType.a.asType) match {
      case ('[r], '[e], '[a]) =>
        val computedTypeMsg = s"Computed Type: ${Format.TypeOf[ZIO[r, e, a]]}"

        if (instructions.info.showComputedType)
          ownerPositionOpt match {
            case Some(pos) =>
              report.info(computedTypeMsg, pos)
            case None =>
              report.info(computedTypeMsg)
          }
        '{ $output.asInstanceOf[ZIO[r, e, a]] }
    }
  }
}