package io.monadless.core

import scala.quoted._
import zio.Task
import io.monadless.core.metaprog.Extractors._
import io.monadless.core.metaprog._
import io.monadless._

class Transformer(using Quotes) {
  import quotes.reflect._

  object Transform {
    def apply(expr: Expr[_]): Expr[_] =
      unapply(expr.asTerm.underlyingArgument.asExpr).getOrElse(expr)

    // TODO really use underlyingArgument????
    def unapply(expr: Expr[_]): Option[Expr[_]] = {
      expr match {
        case Unseal(PureTree(tree)) => None

        case Unseal(Block(parts, lastPart)) =>
          println(s"============  Block: ${parts.map(_.show)} ==== ${lastPart.show}")
            TransformBlock(parts :+ lastPart)

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

    def apply(monad: Expr[_], nestType: NestType, body: Expr[_]): Expr[_] =
      def spliceBody =
        nestType match
          case NestType.ValDef(symbol) => Extractors.Lambda1.fromValDef(symbol, body)
          case NestType.Wildcard => body

      body match {
        // q"${Resolve.flatMap(monad.pos, monad)}(${toVal(name)} => $body)"
        case Transform(body) =>
          '{ ${monad.asExprOf[Task[_]]}.flatMap(v =>
            ${ spliceBody }.asInstanceOf[Any => Task[_]].apply(v)
          ) }

        // q"${Resolve.map(monad.pos, monad)}(${toVal(name)} => $body)"
        case body            =>
          '{ ${monad.asExprOf[Task[_]]}.map(v =>
            ${ spliceBody }.asInstanceOf[Any => _].apply(v)
          ) }
      }
  }

  private object TransformBlock {
    def apply(parts: List[Statement]) =
      parts match {
        case ValDefStatement(symbol , Seal(Transform(monad))) :: tail =>
          println(s"============= Block - With body transform: ${monad.show}")
          Some(Nest(monad, Nest.NestType.ValDef(symbol), BlockN(tail).asExpr))

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
              println(s"============= Block - With multiple terms: ${monad}, ${list.map(_.show)}")
              Some(Nest(monad, Nest.NestType.Wildcard, BlockN(tail).asExpr))
          }

        // Throw error if it is a non-term?
        case other =>
          // TODO fail if there is an unlift here! definitely a problem
          println(s"============= NO BLOCK MATCHED: ${other.map(_.show)}")
          None
      }


  }

  private object BlockN {
    def apply(trees: List[Statement]) =
      trees match {
        case Nil => Block(Nil, '{ () }.asTerm)
        case MatchTerm(head) :: Nil => Block(Nil, head)
        case list if (MatchTerm.unapply(list.last).isDefined) =>
          Block(list.dropRight(1), MatchTerm.unapply(list.last).get)
        case _ => report.throwError("Invalid trees list")
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