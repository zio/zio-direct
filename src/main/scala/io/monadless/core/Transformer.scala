package io.monadless.core

import scala.quoted._
import zio.Task
import io.monadless.core.metaprog.Extractors._
import io.monadless.core.metaprog._
import io.monadless._
import io.monadless.core.util.Format


class Transformer(using transformerQuotes: Quotes) {
  import quotes.reflect._

  object Transform {
    def apply(expr: Expr[_]): Expr[_] =
      unapply(expr.asTerm.underlyingArgument.asExpr).getOrElse(expr)

    // TODO really use underlyingArgument????
    def unapply(expr: Expr[_]): Option[Expr[_]] = {
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
      ret //.map(r => '{ $r.asInstanceOf[Task[Any]] })
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

    def apply(monad: Expr[_], nestType: NestType, bodyRaw: Expr[_]): Expr[_] = {
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
            case '[Task[t]] =>
              '{ ${monad.asExprOf[Task[t]]}.flatMap(v =>
                ${replaceSymbolInBody(body.asTerm)(('v).asTerm).asExprOf[Task[?]]}
              ) }

        // q"${Resolve.map(monad.pos, monad)}(${toVal(name)} => $body)"
        case body            =>
          println(s"=================== Mapping: ${Format(Printer.TreeShortCode.show(body.asTerm))}")
          monad.asTerm.tpe.asType match
            case '[Task[t]] =>
              '{ ${monad.asExprOf[Task[t]]}.map(v =>
                ${replaceSymbolInBody(body.asTerm)(('v).asTerm).asExpr}
              ) }
      }
    }
  }

  private object TransformBlock {
    def unapply(block: Block): Option[Expr[_]] = // todo a zio object?
      val Block(head, tail) = block
      val parts = head :+ tail
      parts match {
        case ValDefStatement(symbol , Seal(Transform(monad))) :: tail =>
          println(s"============= Block - Val Def: ${monad.show}")
          val nest = Nest(monad, Nest.NestType.ValDef(symbol), BlockN(tail).asExpr)
          Some(nest)

        // TODO Validate this?
        //case MatchValDef(name, body) :: tail =>
        //  report.throwError(s"===== validef match bad body: ${body.show}")

        // other statements possible including ClassDef etc... should look into that

        case MatchTerm(Seal(Transform(monad))) :: tail =>
          tail match {
            case Nil =>
              println(s"============= Block - With zero terms: ${monad.show}")
              Some(monad)
            case list =>
              println(s"============= Block - With multiple terms: ${monad.show}, ${list.map(_.show)}")
              Some(Nest(monad, Nest.NestType.Wildcard, BlockN(tail).asExpr))
          }

        // This is the recursive case of TransformBlock, it will work across multiple things
        // between blocks due to the recursion e.g:
        //   val blah = new Blah(2) // 1st part, will recurse 1st time (here)
        //   import blah._          // 2nd part, will recurse 2nd time (here)
        //   val b = unlift(ZIO.succeed(value).asInstanceOf[Task[Int]]) // Then will match valdef case
        case head :: BlockN(TransformBlock(parts)) =>
          Some(BlockN(List(head, parts.asTerm)).asExpr)

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
      }

    def apply(trees: List[Statement]): Block =
      BlockN.unapply(trees) match {
        case Some(value) => value
        case None => report.throwError(s"Invalid trees list: ${trees.map(_.show)}")
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

  def apply[T: Type](value: Expr[T])(using Quotes): Expr[Task[T]] = {
    '{ ${Transform(value)}.asInstanceOf[Task[T]] }
  }
}