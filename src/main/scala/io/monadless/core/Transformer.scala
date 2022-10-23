package io.monadless.core

import scala.quoted._
import zio.Task
import io.monadless.core.metaprog.Extractors._
import io.monadless.core.metaprog._
import io.monadless._
import io.monadless.core.util.Format
import zio.ZIO


class Transformer(using transformerQuotes: Quotes) {
  import quotes.reflect._

  object Transform {
    def apply(expr: Expr[?]): Expr[ZIO[Any, Throwable, ?]] =
      unapply(expr.asTerm.underlyingArgument.asExpr)
        .getOrElse('{ ZIO.succeed($expr) })

    // TODO really use underlyingArgument????
    def unapply(expr: Expr[?]): Option[Expr[ZIO[Any, Throwable, ?]]] = {
      println("================== UNAPPLY ==================")
      val ret = expr match {
        case Unseal(PureTree(tree)) =>
          println(s"============  Tree is Pure!: ${tree.show}")
          None

        case Unseal(block @ Block(parts, lastPart)) if (parts.nonEmpty) =>
          println(s"============  Block: ${parts.map(_.show)} ==== ${lastPart.show}")
          TransformBlock.unapply(block)

        case '{ unlift[t]($task) } =>
          println(s"=============== Unlift: ${task.show}")
          Some(task)

        case Unseal(Typed(tree, _)) =>
          println(s"=============== Untype: ${tree.show}")
          unapply(tree.asExpr)

        case other =>
          println(s"=============== Other: ${other.show}")
          None
      }
      println("================== DONE UNAPPLY ==================")
      ret
    }
  }

  private object PureTree:
    def unapply(tree: Tree) =
      Trees.exists(tree, Symbol.spliceOwner) {
        case Seal('{ unlift[t]($v) }) => true
      } match {
        case true => None
        case false => Some(tree)
      }

  private object Nest {
    enum NestType:
      case ValDef(symbol: Symbol)
      case Wildcard

    /**
      * This will actually take some block of code (that is either in a ZIO or just 'pure' code)
      * and will nest it into the previously-sequenced ZIO. If it is ZIO code, it will
      * flatMap from the previously-sequenced ZIO, otherwise it map.
      */
    def apply(monad: Expr[ZIO[Any, Throwable, ?]], nestType: NestType, bodyRaw: Expr[_]): Expr[ZIO[Any, Throwable, ?]] = {
      def replaceSymbolInBody(body: Term)(newSymbolTerm: Term) =
        nestType match
          case NestType.ValDef(oldSymbol) =>
            /**
             * In a case where we have:
             *  val a = unlift(foobar)
             *  otherStuff
             *
             * We can either lift that into:
             *  unlift(foobar).flatMap { v => (otherStuff /*with replaced a -> v*/) }
             *
             * Or we can just do
             *  unlift(foobar).flatMap { v => { val a = v; otherStuff /* with the original a variable*/ } }
             *
             * I think the 2nd variant more performant but keeping 1st one (Trees.replaceIdent(...)) around for now.
             */
            //Trees.replaceIdent(using transformerQuotes)(body)(oldSymbol, newSymbolTerm.symbol)

            val out =
              BlockN(List(
                ValDef(oldSymbol, Some(newSymbolTerm)),
                body
              ))
            println(s"============+ Creating $oldSymbol -> ${newSymbolTerm.symbol} replacement let:\n${Format(Printer.TreeShortCode.show(out))}")
            out

          case NestType.Wildcard =>
            body

      bodyRaw match {
        // q"${Resolve.flatMap(monad.pos, monad)}(${toVal(name)} => $body)"
        case Transform(body) =>
          println(s"=================== Flat Mapping: ${Format(Printer.TreeShortCode.show(body.asTerm))}")
          monad.asTerm.tpe.asType match
            case '[ZIO[Any, Throwable, t]] =>
              '{ ${monad.asExprOf[ZIO[Any, Throwable, t]]}.flatMap(v =>
                ${replaceSymbolInBody(body.asTerm)(('v).asTerm).asExprOf[ZIO[Any, Throwable, ?]]}
              ) }

        // q"${Resolve.map(monad.pos, monad)}(${toVal(name)} => $body)"
        case body            =>
          println(s"=================== Mapping: ${Format(Printer.TreeShortCode.show(body.asTerm))}")
          monad.asTerm.tpe.asType match
            case '[ZIO[Any, Throwable, t]] =>
              '{ ${monad.asExprOf[ZIO[Any, Throwable, t]]}.map(v =>
                ${replaceSymbolInBody(body.asTerm)(('v).asTerm).asExpr}
              ) }
      }
    }
  }

  /**
    * Transform a sequence of steps
    * a; b = unlift(zio); c
    * Into a.flatMap()
    */
  private object TransformBlock {
    def unapply(block: Block): Option[Expr[ZIO[Any, Throwable, ?]]] =
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
        case ValDefStatement(symbol , Seal(Transform(monad))) :: tail =>
          println(s"============= Block - Val Def: ${monad.show}")
          val nest = Nest(monad, Nest.NestType.ValDef(symbol), BlockN(tail).asExpr)
          Some(nest.asExprOf[ZIO[Any, Throwable, ?]])

        // TODO Validate this?
        //case MatchValDef(name, body) :: tail =>
        //  report.throwError(s"===== validef match bad body: ${body.show}")

        // other statements possible including ClassDef etc... should look into that

        case MatchTerm(Seal(Transform(monad))) :: tail =>
          tail match {
            // In this case where is one statement in the block which my definition
            // needs to have the same type as the output: e.g.
            //   val v: T = { unlift(doSomething:ZIO[_, _, T]) }
            // since we've pulled out the `doSomething` inside the signature
            // will be ZIO[_, _, T] instead of T.
            case Nil =>
              println(s"============= Block - With zero terms: ${monad.show}")
              Some(monad.asExprOf[ZIO[Any, Throwable, ?]])
            case list =>
              // In this case there are multiple instructions inside the seauence e.g:
              //   val v: T = { unlift(x), y /*pure*/, unlift(z:ZIO[_, _, T]) }
              // We recurse by flatMapping x, y, and unlift... but eventually the type
              // also has to be ZIO[_, _, T] since we are essentially doing:
              // x.flatMap(.. -> {y; z: ZIO[_, _, T]}). Of course the value will be ZIO[_, _, T]
              // since the last value of a nested flatMap chain is just the last instruction
              // in the nested sequence.
              println(s"============= Block - With multiple terms: ${monad.show}, ${list.map(_.show)}")
              val nest = Nest(monad, Nest.NestType.Wildcard, BlockN(tail).asExpr)
              Some(nest.asExprOf[ZIO[Any, Throwable, ?]])
          }

        // This is the recursive case of TransformBlock, it will work across multiple things
        // between blocks due to the recursion e.g:
        //   val blah = new Blah(2) // 1st part, will recurse 1st time (here)
        //   import blah._          // 2nd part, will recurse 2nd time (here)
        //   val b = unlift(ZIO.succeed(value).asInstanceOf[Task[Int]]) // Then will match valdef case
        case head :: BlockN(TransformBlock(parts)) =>
          Some(BlockN(List(head, parts.asTerm)).asExprOf[ZIO[Any, Throwable, ?]])

        // Throw error if it is a non-term?
        case other =>
          // TODO fail if there is an unlift here! definitely a problem
          println(s"============= NO BLOCK MATCHED: ${other.map(_.show)}")
          None
      }


  }

  private object BlockN {
    def unapply(trees: List[Statement]) =
      trees match {
        case Nil => None
        case MatchTerm(head) :: Nil =>
          Some(Block(Nil, head))
        case list if (MatchTerm.unapply(list.last).isDefined) =>
          Some(Block(list.dropRight(1), MatchTerm.unapply(list.last).get))
        case _ =>
          report.errorAndAbort(s"Last element in the instruction group is not a block. ${trees.map(_.show)}")
      }

    def apply(trees: List[Statement]): Block =
      BlockN.unapply(trees) match {
        case Some(value) => value
        case None => report.errorAndAbort(s"Invalid trees list: ${trees.map(_.show)}")
      }
  }

  private object Lambda1 {
    def apply(newOwner: Term)(name: String, body: Expr[_])(input: TypeRepr, output: TypeRepr) = {
      val mtpe = MethodType(List(name))(_ => List(input), _ => output)
      Lambda(newOwner.symbol, mtpe, {
        case (methSym, List(name: Term)) =>
          //given Quotes = methSym.asQuotes
          body.asTerm.changeOwner(methSym)
        }
      ).asExprOf[? => ?]
    }
  }

  def apply[T: Type](value: Expr[T])(using Quotes): Expr[ZIO[Any, Throwable, T]] = {
    '{ ${Transform(value)}.asInstanceOf[ZIO[Any, Throwable, T]] }
  }
}