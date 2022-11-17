package zio.direct.core.metaprog

import scala.quoted._
import scala.quoted.Varargs
import zio.direct.core.util.Format
import zio.ZIO

object Extractors {
  import zio.direct._

  object RunCall {
    def unapply(using Quotes)(tree: quotes.reflect.Tree): Option[Expr[ZIO[?, ?, ?]]] =
      import quotes.reflect._
      tree match
        case Seal('{ run[r, e, a]($task) })       => Some(task)
        case Seal('{ ($task: ZIO[r, e, a]).run }) => Some(task)
        case _                                    => None
  }

  extension [T: Type](expr: Expr[T])
    def reseal(using Quotes): Expr[T] =
      import quotes.reflect._
      expr.asTerm.underlyingArgument.asExprOf[T]

  private object TypedMatroshkaTerm {
    def recurse(using Quotes)(innerTerm: quotes.reflect.Term): quotes.reflect.Term =
      import quotes.reflect._
      innerTerm match
        case Typed(innerTree, _) => recurse(innerTree)
        case other               => other

    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
      import quotes.reflect._
      term match
        case Typed(tree, _) => Some(recurse(tree))
        case other          => None
  }

  object BlockN {
    def unapply(using Quotes)(trees: List[quotes.reflect.Statement]) =
      import quotes.reflect._
      trees match {
        case Nil => None
        case IsTerm(head) :: Nil =>
          Some(Block(Nil, head))
        case list if (IsTerm.unapply(list.last).isDefined) =>
          Some(Block(list.dropRight(1), IsTerm.unapply(list.last).get))
        case _ =>
          report.errorAndAbort(s"Last element in the instruction group is not a block. ${trees.map(_.show)}")
      }

    def apply(using Quotes)(trees: List[quotes.reflect.Statement]): quotes.reflect.Block =
      import quotes.reflect._
      BlockN.unapply(trees) match {
        case Some(value) => value
        case None        => report.errorAndAbort(s"Invalid trees list: ${trees.map(_.show)}")
      }
  }

  object IsTerm:
    def unapply(using Quotes)(value: quotes.reflect.Tree): Option[quotes.reflect.Term] =
      import quotes.reflect._
      value match {
        case term: Term => Some(term)
        case other      => None
      }

  object Unseal {
    def unapply(using Quotes)(t: Expr[Any]): Option[quotes.reflect.Term] =
      import quotes.reflect._
      Some(t.asTerm)
  }
  object Seal {
    def unapply(using Quotes)(e: quotes.reflect.Tree) = {
      import quotes.reflect._
      e match
        // Some terms coming from tree-expressions actual cannot be converted to Exprs
        // use t.isExpr to check that
        case t: Term if (t.isExpr) => Some(t.asExpr)
        case _                     => None
    }
  }

  object SymbolOps {
    def isSynthetic(using Quotes)(s: quotes.reflect.Symbol) = isSyntheticName(getName(s))
    private def isSyntheticName(name: String) = {
      name == "<init>" || (name.startsWith("<local ") && name.endsWith(">")) || name == "$anonfun" || name == "macro"
    }
    private def getName(using Quotes)(s: quotes.reflect.Symbol) = {
      s.name.trim
        .stripSuffix("$") // meh
    }
  }

  def is[T: Type](using Quotes)(expr: Expr[_]) =
    import quotes.reflect._
    expr.asTerm.tpe <:< TypeRepr.of[T]
}
