package io.monadless.core

import scala.quoted._
import zio.Task
import io.monadless.core.metaprog.Extractors._
import io.monadless.core.metaprog._
import io.monadless._
import io.monadless.core.util.Format


class Transformer(using Quotes) {
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

        case Unseal(Block(parts, lastPart)) if (parts.nonEmpty) =>
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

    def apply(monad: Expr[_], nestType: NestType, bodyRaw: Expr[_]): Expr[_] =
      val (spliceBody, isFlatMap) =
        bodyRaw match
          case Transform(body) => (body, true)
          case body => (body, false)

      isFlatMap match {
        // q"${Resolve.flatMap(monad.pos, monad)}(${toVal(name)} => $body)"
        case true =>
          println(s"=================== Flat Mapping: ${Format(Printer.TreeShortCode.show(spliceBody.asTerm))}")
          '{ ${monad.asExprOf[Task[?]]}.flatMap(v =>
            ${
              (nestType match {
                case NestType.ValDef(symbol) =>
                  (new TreeMap:
                    override def transformTerm(tree: Term)(owner: Symbol): Term = {
                      tree match
                        case id: Ident if (id.symbol == symbol) =>
                          val newIdent = Ident(('v).asTerm.symbol.termRef)
                          println(s">>>>>>>>>> Transforming $id -> $newIdent")
                          newIdent
                        case other =>
                          super.transformTerm(other)(symbol.owner)
                    }
                  ).transformTerm(spliceBody.asTerm)(symbol.owner).asExpr
                case NestType.Wildcard =>
                  spliceBody
              }).asExprOf[Task[?]]
            }
          ) }

        // q"${Resolve.map(monad.pos, monad)}(${toVal(name)} => $body)"
        case false            =>
          println(s"=================== Mapping: ${Format(Printer.TreeShortCode.show(spliceBody.asTerm))}")
          '{ ${monad.asExprOf[Task[?]]}.map(v =>
            ${
              nestType match {
                case NestType.ValDef(symbol) =>
                  (new TreeMap:
                    override def transformTerm(tree: Term)(owner: Symbol): Term = {
                      tree match
                        case id: Ident if (id.symbol == symbol) =>
                          val newIdent = Ident(('v).asTerm.symbol.termRef)
                          println(s">>>>>>>>>> Transforming $id -> $newIdent")
                          newIdent
                        case other =>
                          super.transformTerm(other)(symbol.owner)
                    }
                  ).transformTerm(spliceBody.asTerm)(symbol.owner).asExpr
                case NestType.Wildcard =>
                  spliceBody
              }
            }
          ) }
      }

    def fromValDef(using Quotes)(symbol: quotes.reflect.Symbol, inputType: quotes.reflect.TypeRepr, bodyRaw: Expr[_]) =
      import quotes.reflect._
      val mtpe = MethodType(List("arg1"))(_ => List(inputType), _ => TypeRepr.of[Any])
      var isTransformed = false
      val lam =
        Lambda(symbol.owner, mtpe, {
            case (methSym, List(arg1: Term)) =>
              //given Quotes = methSym.asQuotes


              val newBody =
                (new TreeMap:
                  override def transformTerm(tree: Term)(owner: Symbol): Term = {
                    tree match
                      case id: Ident if (id.symbol == symbol) =>
                        //println(s"============+ REPLACEING IDENT OF: ${bodyRaw.show}")
                        //val newSym = Symbol.newMethod(owner, arg1.symbol.name, arg1.tpe.widen)
                        val newIdent = Ident(arg1.symbol.termRef)
                        println(s">>>>>>>>>> Transforming $id -> $newIdent")
                        newIdent
                      case other =>
                        super.transformTerm(other)(symbol.owner)
                  }
                ).transformTerm(bodyRaw.asTerm)(symbol.owner)

              println(s"++++++++++ Replaced: ${symbol} with ${arg1.symbol} in\n${Format(newBody.show)}")


              println(s"================ HERE: ${newBody.show} =================")
              newBody.asExpr match {
                case Transform(body) =>
                  isTransformed = true
                  println(s"================ (OUTPUT: ${bodyRaw.asTerm.tpe.show}) TRANSFORM: ${body.show} =================")
                  body.asTerm
                case body =>
                  println(s"================ NO TRANSFORM: ${body.show} =================")
                  body.asTerm
              }

            case other =>
              report.throwError(s"Invalid valdef: ${other}")
          }
        )
      ('{ ${lam.asExpr}.asInstanceOf[? => ?] }, isTransformed)
  }

  private object TransformBlock {
    def apply(parts: List[Statement]) =
      parts match {
        case ValDefStatement(symbol , Seal(Transform(monad))) :: tail =>
          println(s"============= Block - Val Def: ${monad.show}")
          val nest = Nest(monad, Nest.NestType.ValDef(symbol), BlockN(tail).asExpr)
          println("=========== DONE FLATMAPPING ========")
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