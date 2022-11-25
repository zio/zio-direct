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
        case Seal('{ run[r, e, a]($task) }) =>
          println(s"-------- Detect run{ ... } call: ${Format.Expr(task)}")
          Some(task)
        case Seal('{ ($task: ZIO[r, e, a]).run }) =>
          println(s"-------- Detect .run call: ${Format.Expr(task)}")
          Some(task)
        case _ => None
  }

  def firstParamList(using Quotes)(applyNode: quotes.reflect.Apply) =
    import quotes.reflect._
    val fn = applyNode.fun
    for {
      methodSym <- if (fn.symbol.flags.is(Flags.Method)) Some(fn.symbol) else None
      firstParamList <- fn.symbol.paramSymss.find(params => params.headOption.exists(_.isValDef))
    } yield firstParamList

  object ImplicitArgs {
    sealed trait ArgType {
      def isImplicit: Boolean
    }
    object ArgType {
      case object Implicit extends ArgType { val isImplicit = true }
      case object Regular extends ArgType { val isImplicit = false }
    }

    // Get the arguments from the unapply if they are not implicit. Since implicit-ness is typically defined
    // on the clause (I think in some rare cases the term itself can be implicit) so if the function-clause
    // of the Apply is implicit then this list will be empty. Otherwise, it will consist of all the arugments.
    def fromFunctionMarked(using Quotes)(applyNode: quotes.reflect.Apply) = {
      import quotes.reflect._
      val firstParams = Extractors.firstParamList(applyNode)
      val Apply(_, argsRaw) = applyNode

      // Sometimes we don't know directly from the Term whether it is implicit or not, try to get the
      // arg val-defs and see if they are marked implicit there. All of this needs to be added to the code
      // formatting logic.
      val argsJoined: List[(Term, Option[Symbol])] =
        firstParams match {
          case Some(parmValDefs) if parmValDefs.length == argsRaw.length =>
            argsRaw.zip(parmValDefs.map(Some(_)))
          case _ =>
            argsRaw.map(arg => (arg, None))
        }

      argsJoined.map((arg, argValDef) => {
        val isTermImplict = arg.symbol.flags.is(Flags.Given) || arg.symbol.flags.is(Flags.Implicit)
        // Note that if there is no argValDef we treat the term as non-implicit
        val isValDefImplicit = argValDef.exists(vd => vd.flags.is(Flags.Given) || vd.flags.is(Flags.Implicit))
        val isImplicit = isTermImplict || isValDefImplicit
        (arg, { if (isImplicit) ArgType.Implicit else ArgType.Regular })
      })
    }
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

  object Lambda1 {
    def unapply(using Quotes)(expr: Expr[_]): Option[(quotes.reflect.Symbol, quoted.Expr[_])] =
      import quotes.reflect._
      Lambda1.Term.unapply(expr.asTerm).map((sym, expr) => (sym, expr.asExpr))

    object Term {
      def unapply(using Quotes)(term: quotes.reflect.Term): Option[(quotes.reflect.Symbol, quotes.reflect.Term)] =
        import quotes.reflect._
        Untype(term) match {
          case Lambda(List(vd @ ValDef(ident, tpeTree, _)), methodBody) => Some((vd.symbol, methodBody))
          case _                                                        => None
        }
    }
  }

  object Untype {
    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] = term match {
      case TypedMatroshkaTerm(t) => Some(t)
      case other                 => Some(other)
    }

    def apply(using Quotes)(term: quotes.reflect.Term) = Untype.unapply(term).get
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
