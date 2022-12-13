package zio.direct.core.util

import scala.reflect.macros.blackbox

/**
 * removes visual clutter from scala reflect Trees.
 */

private[zio] object CleanCodePrinter {
  private val magicQuote = "-- $%^*"
  private val startQuote = s"`$magicQuote"
  private val endQuote = s"$magicQuote`"
  private val magicArg = "x$$$$123"
  private val tagRegex = "\\(Tag.+?$".r.regex

  def show(c: blackbox.Context)(expr: c.Tree, showDetails: ShowDetails): String = {
    import c.universe._
    postProcess(showCode(clean(c)(cleanImplicits(c)(expr, showDetails), CleanContext())))
    // showCode(expr)
  }

  private def postProcess(code: String): String =
    code
      .replace(startQuote, "\"")
      .replace(endQuote, "\"")
      .replace(s"($magicArg) => ", "")
      .replaceAll(tagRegex, "")

  private case class CleanContext(funcSyntheticArgs: Set[String] = Set.empty) {
    def withFuncSyntheticArgs(args: Set[String]): CleanContext = copy(funcSyntheticArgs = args)
  }

  private def clean(c: blackbox.Context)(expr: c.Tree, ctx: CleanContext): c.Tree = {
    import c.universe._
    object PackageSelects {
      def unapply(tree: c.Tree): Option[String] = packageSelects(c)(tree)
    }
    expr match {
      // TODO figure out a way to hide implicit classes in general (the quill parser might have something for this)
      // remove type parameters from methods: foo[Int](args) => foo(args)
      case Apply(TypeApply(t, _), args) => Apply(clean(c)(t, ctx), cleanArgs(c)(args, ctx))
      case Apply(t, args)               => Apply(clean(c)(t, ctx), cleanArgs(c)(args, ctx))
      // foo.apply => foo
      case Select(PackageSelects(n), TermName("apply")) => Ident(TermName(cleanTupleTerm(n)))
      case Select(PackageSelects(n), TermName(name))    => Select(Ident(TermName(n)), TermName(cleanTupleTerm(name)))
      case PackageSelects(n)                            => Ident(TermName(cleanTupleTerm(n)))
      case Select(This(_), tn)                          => Ident(tn)
      case Select(left, TermName("apply"))              => clean(c)(left, ctx)
      case Select(t, n)                                 => Select(clean(c)(t, ctx), n)
      case Ident(TermName(n))                           => Ident(TermName(nameOrUnderscoreArg(n, ctx)))

      case l @ Literal(Constant(s: String)) =>
        if (s.contains("\n")) Ident(TermName(s"$magicQuote${s.replace("\n", "\\n")}$magicQuote"))
        else l
      case Typed(tree, _)      => clean(c)(tree, ctx)
      case Throw(tree)         => Throw(clean(c)(tree, ctx))
      case New(tree)           => New(clean(c)(tree, ctx))
      case CaseDef(t1, t2, t3) => CaseDef(clean(c)(t1, ctx), clean(c)(t2, ctx), clean(c)(t3, ctx))
      case Match(tree, cases) =>
        Match(
          clean(c)(tree, ctx),
          cases.map { cd =>
            (cd: @unchecked) match {
              case CaseDef(t1, t2, t3) => CaseDef(clean(c)(t1, ctx), clean(c)(t2, ctx), clean(c)(t3, ctx))
            }
          }
        )
      case Block(trees, tree) => Block(trees.map(clean(c)(_, ctx)), clean(c)(tree, ctx))
      case If(t1, t2, t3)     => If(clean(c)(t1, ctx), clean(c)(t2, ctx), clean(c)(t3, ctx))
      case Bind(n, t)         => Bind(n, clean(c)(t, ctx))
      case Function(vals, tree) =>
        val (synthetic, nonSynthetic) = vals.partition(_.mods.hasFlag(Flag.SYNTHETIC))
        val newArgs =
          if (nonSynthetic.isEmpty) List(ValDef(Modifiers(Flag.PARAM), TermName(magicArg), EmptyTree, EmptyTree))
          else cleanValDefs(c)(nonSynthetic)
        Function(
          newArgs,
          clean(c)(
            tree,
            ctx.withFuncSyntheticArgs(synthetic.collect { case ValDef(_, TermName(name), _, _) =>
              name
            }.toSet)
          )
        )
      case t => t
    }
  }

  private def cleanTupleTerm(n: String) =
    if (n.matches("Tuple\\d+")) "" else n

  private def cleanValDefs(c: blackbox.Context)(vals: List[c.universe.ValDef]) = {
    import c.universe._
    vals.map { vd =>
      (vd: @unchecked) match {
        case ValDef(mods, name, _, _) =>
          ValDef(mods, name, EmptyTree, EmptyTree)
      }
    }
  }

  private def nameOrUnderscoreArg(n: String, ctx: CleanContext) =
    if (ctx.funcSyntheticArgs(n)) "_"
    else n

  private def cleanArgs(c: blackbox.Context)(args: List[c.Tree], ctx: CleanContext): List[c.Tree] = {
    import c.universe._
    args.map(clean(c)(_, ctx)).filter {
      case Ident(TermName(name))     => !name.matches(".*\\$default\\$\\d+$")
      case Select(_, TermName(name)) => !name.matches(".*\\$default\\$\\d+$")
      case _                         => true
    }
  }

  private def packageSelects(c: blackbox.Context)(select: c.universe.Tree): Option[String] = {
    import c.universe._
    select match {
      case Select(id @ Ident(_), n: Name) if id.symbol.isPackage => Some(n.decodedName.toString)
      case Select(nested @ Select(_, _), n: Name)                => packageSelects(c)(nested).map(_ => n.decodedName.toString)
      case _                                                     => None
    }
  }

  /**
   * Remove all implicit parameter lists from the Tree
   */
  private def cleanImplicits(c: blackbox.Context)(expr: c.Tree, showDetails: ShowDetails): c.Tree = {
    import c.universe._

    def findParamLists(applyNode: Apply) = {
      val fn = applyNode.fun
      for {
        methodSym <- if (fn.symbol.isMethod) Some(fn.symbol) else None
        firstParamList <- Some(fn.symbol.asMethod.paramLists) // .find(params => params.headOption.exists(_.isParameter))
      } yield firstParamList
    }

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

      def fromFunctionMarked(applyNode: Apply, paramList: Option[List[Symbol]]) = {
        val firstParams = paramList
        val argsRaw = applyNode.args
        // println(s"============ Args List: ${argsRaw}")

        // Sometimes we don't know directly from the Term whether it is implicit or not, try to get the
        // arg val-defs and see if they are marked implicit there. All of this needs to be added to the code
        // formatting logic.
        val argsJoined: List[(Tree, Option[Symbol])] =
          firstParams match {
            case Some(parmValDefs) if (parmValDefs.length == argsRaw.length) =>
              argsRaw.zip(parmValDefs.map(Some(_)))
            case _ =>
              argsRaw.map(arg => (arg, None))
          }

        argsJoined.map {
          case (arg, argValDef) => {
            val isValDefImplicit = argValDef.exists(vd => vd.isImplicit)
            (arg, { if (isValDefImplicit) ArgType.Implicit else ArgType.Regular })
          }
        }
      }
    }

    new Transformer {
      private def transformNestedApply(applyNode: Apply, paramListsOpt: Option[List[List[Symbol]]]): Tree = {
        val lastParamListOpt = paramListsOpt.map(_.lastOption).flatten
        val newArgs =
          if (!showDetails.showImplicitClauses)
            ImplicitArgs.fromFunctionMarked(applyNode, lastParamListOpt).filter { case (_, stat) => !stat.isImplicit }.map(_._1)
          else {
            // Recurse on the arguments and do not filter, in the future we might want to add additional logic that overrides
            // this for certain things (e.g. hide ZIO traces even if showImplicitClausese is true) so delving into the arguments
            // even in the showImplicitClauses mode.
            ImplicitArgs.fromFunctionMarked(applyNode, lastParamListOpt).map(_._1)
          }

        val newFun =
          applyNode.fun match {
            case innerApply @ Apply(_, _) =>
              transformNestedApply(innerApply, paramListsOpt.map(_.dropRight(1)))
            case other =>
              transform(other)
          }

        if (newArgs.length > 0)
          Apply(newFun, newArgs.map(arg => transform(arg)))
        else
          newFun
      }

      override def transform(tree: Tree) =
        tree match {
          case applyNode @ Apply(t, args) /*if args.exists(t => (t.tpe <:< tracerType || t.tpe <:< tagType))*/ =>
            val paramLists = findParamLists(applyNode)
            transformNestedApply(applyNode, paramLists)

          // Apply(transform(t), args)
          case other =>
            super.transform(tree)
        }
    }.transform(expr)
  }
}
