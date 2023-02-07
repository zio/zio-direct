package zio.direct.core.metaprog

import scala.quoted._
import scala.quoted.Varargs
import zio.direct.core.util.Format
import zio.Chunk

object Extractors {
  import zio.direct._

  // class RunCallExtractor[F[_, _, _]: Type] {
  //   // TODO need to refactor API here. Maybe match whatever function is annotated in a certain way?
  //   //      possibly need to detect extensions methods on a tree-level.
  //   //      Or maybe just make the `run` call just be generic F[a, b, c] => c ? Need to discuss.
  //   def unapply(using Quotes)(tree: quotes.reflect.Tree): Option[Expr[_]] =
  //     import quotes.reflect._
  //     tree match
  //       case Seal('{ run[r, e, a]($task) }) =>
  //         Some(task.asExprOf[F[r, e, a]])
  //       case Seal('{ ($task: ZIO[r, e, a]).run }) =>
  //         Some(task.asExprOf[F[r, e, a]])
  //       case Seal('{ ($task: ZStream[r, e, a]).run }) =>
  //         Some(task.asExprOf[F[r, e, a]])
  //       case _ => None
  // }

  // class HasAnnotation(name: String) {
  //   object Term {
  //     def apply(using Quotes)(term: quotes.reflect.Term) = {
  //       import quotes.reflect._
  //       val termUntyped = Untype(term)
  //       AnnotationsOf.Symbol(termUntyped.tpe.termSymbol)
  //     }
  //   }

  //   object Symbol {
  //     def apply(using Quotes)(symbol: quotes.reflect.Symbol) = {
  //       import quotes.reflect._
  //       symbol.annotations.collect {
  //         case UnApplys(UntypeApply(Select(New(id: TypeIdent), "<init>")), argss) =>
  //           (id.tpe.typeSymbol.fullName, argss)
  //       }
  //     }
  //   }
  // }

  /**
   * Agnostic to Apply(Apply(Apply(term, args1), args2), args3). If there are apply nodes,
   * it will return Some((term, Some(List(args1, args2, args3)))) otherwise it will return
   * Some((term, None))
   */
  object UnApplys {
    private def recurse(using Quotes)(term: quotes.reflect.Term, accum: List[List[quotes.reflect.Term]]): (quotes.reflect.Term, List[List[quotes.reflect.Term]]) = {
      import quotes.reflect._
      term match {
        // prepend every args list instead of append, then reverse at the end
        case Apply(inner, args) => recurse(inner, args +: accum)
        case _                  => (term, accum.reverse)
      }
    }

    def unapply(using Quotes)(term: quotes.reflect.Term): Option[(quotes.reflect.Term, Option[List[List[quotes.reflect.Term]]])] = {
      import quotes.reflect._
      term match {
        case Apply(inner, args) =>
          val (root, allArgs) = recurse(inner, List(args))
          Some(root, Some(allArgs))
        case _ =>
          Some((term, None))
      }
    }
  }

  object UntypeApply {
    private def recurse(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term = {
      import quotes.reflect._
      term match {
        case TypeApply(terms, typeArgs) => recurse(terms)
        case other                      => other
      }
    }
    def unapply(using Quotes)(term: quotes.reflect.Term) = Some(recurse(term))
  }

  object AnyGetCall {
    def unapply(using Quotes)(tree: quotes.reflect.Tree): Boolean =
      import quotes.reflect._

      // val isZeroArg =
      //   tree match {
      //     case DottyFunctionCall.ZeroArg(invocation) =>
      //       s"invocation-match: ${Printer.TreeStructure.show(invocation)}\n" +
      //         s"Annotations:\n${invocation.tpe.termSymbol.annotations.map(annot => Printer.TreeStructure.show(annot))}\n"

      //     case _ =>
      //       "invocation-fail"
      //   }
      // println(
      //   s"----------- here: ${Printer.TreeStructure.show(tree)} - ${isZeroArg} -------------"
      // )

      tree match
        case DottyFunctionCall.ZeroArg(invocation @ DirectGetCallAnnotated.Term()) =>
          // println("match")
          true
        case _ =>
          // println("fail")
          false
  }
  object AnySetCall {
    def unapply(using Quotes)(tree: quotes.reflect.Tree): Option[Expr[_]] =
      tree match
        case DottyFunctionCall.OneArg(invocation @ DirectSetCallAnnotated.Term(), arg) => Some(arg.asExpr)
        case _                                                                         => None
  }
  object AnyLogCall {
    def unapply(using Quotes)(tree: quotes.reflect.Tree): Option[Expr[_]] =
      tree match
        case DottyFunctionCall.OneArg(invocation @ DirectLogCallAnnotated.Term(), arg) => Some(arg.asExpr)
        case _                                                                         => None
  }
  object AnyUtilityCall {
    def unapply(using Quotes)(tree: quotes.reflect.Tree): Boolean =
      tree match
        case AnyGetCall()  => true
        case AnySetCall(_) => true
        case AnyLogCall(_) => true
        case _             => false
  }

  object AnyRunCall {
    def unapply(using Quotes)(tree: quotes.reflect.Tree): Option[Expr[_]] = {
      tree match
        case DottyExtensionCall.OneArg(invocation @ DirectRunCallAnnotated.Term(), effect) =>
          Some(effect.asExpr)
        case _ =>
          None
    }
  }

  /* Completely specific therefore maximally efficient match of directRunCall */
  object DirectRunCallAnnotated extends AnnotatedCall("directRunCall")
  object DirectSetCallAnnotated extends AnnotatedCall("directSetCall")
  object DirectGetCallAnnotated extends AnnotatedCall("directGetCall")
  object DirectLogCallAnnotated extends AnnotatedCall("directLogCall")

  class AnnotatedCall(annotationName: String) { self =>
    object Term {
      def unapply(using Quotes)(term: quotes.reflect.Term): Boolean = {
        import quotes.reflect._
        // early-exist if it's the wrong effect-type
        val termUntyped = Untype(term)
        self.Symbol.unapply(termUntyped.tpe.termSymbol)
      }
    }

    object Symbol {
      def unapply(using Quotes)(symbol: quotes.reflect.Symbol): Boolean = {
        import quotes.reflect._
        symbol.annotations.exists { annot =>
          annot match {
            case v @ Apply(Select(New(typeId), "<init>"), argss) if (typeId.symbol.name == annotationName) =>
              true
            case _ =>
              false
          }
        }
      }
    }
  }

  object DottyExtensionCall extends DottyCall(true)
  object DottyFunctionCall extends DottyCall(false)

  class DottyCall(mustBeExtension: Boolean) {
    private val matchNonExtension = !mustBeExtension

    private object SelectOrIdent {
      def unapply(using Quotes)(term: quotes.reflect.Term): Boolean =
        import quotes.reflect._
        term match
          case _: Select => true
          case _: Ident  => true
          case _         => false
    }

    object OneArg {
      def unapply(using Quotes)(term: quotes.reflect.Term) =
        import quotes.reflect._
        term match
          case Apply(
                UntypeApply(invocation @ SelectOrIdent()),
                List(arg)
              )
              if (matchNonExtension || invocation.tpe.termSymbol.flags.is(Flags.ExtensionMethod)) =>
            Some((invocation, arg))
          case _ =>
            None
    }
    object ZeroArg {
      def unapply(using Quotes)(term: quotes.reflect.Term) =
        import quotes.reflect._
        term match
          // Invoke with zero-args and apply e.g. State.get() and possibly a type-param e.g. State.get[T]
          case Apply(
                UntypeApply(invocation @ SelectOrIdent()),
                Nil
              )
              if (matchNonExtension || invocation.tpe.termSymbol.flags.is(Flags.ExtensionMethod)) =>
            Some(invocation)

          // Invoke with zero-args and without apply e.g. State.get() and possibly a type-param e.g. State.get[T]()
          case UntypeApply(invocation @ SelectOrIdent())
              if (matchNonExtension || invocation.tpe.termSymbol.flags.is(Flags.ExtensionMethod)) =>
            Some(invocation)
          case _ =>
            None
    }
  }

  object NotBlock {
    object Term {
      def unapply(using Quotes)(term: quotes.reflect.Term) =
        import quotes.reflect._
        term match {
          case _: Block                   => None
          case other: quotes.reflect.Term => Some(other)
        }
    }
  }

  object Dealiased {
    def unapply(using Quotes)(repr: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] =
      Some(repr.widenTermRefByName.dealias)
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
