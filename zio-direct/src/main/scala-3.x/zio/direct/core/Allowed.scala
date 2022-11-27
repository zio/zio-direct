package zio.direct.core

import scala.quoted._
import zio.direct.core.util.Format
import zio.direct.core.metaprog.Trees
import zio.direct.core.metaprog.Extractors._
import zio.direct.core.util.PureTree
import zio.direct.core.util.Unsupported
import zio.direct.core.util.Messages
import zio.direct.core.metaprog.Instructions
import zio.direct.core.metaprog.Verify
import zio.direct.core.util.ShowDetails
import zio.direct.core.metaprog.InfoBehavior
import zio.direct.Dsl.Internal.deferred
import zio.direct.Dsl.Internal.ignore

object Allowed {

  object FromMutablePackage {
    def check(using Quotes)(tpe: quotes.reflect.TypeRepr) = {
      import quotes.reflect._
      val fullName = tpe.typeSymbol.fullName
      (
        fullName.startsWith("scala.collection.mutable") ||
          tpe <:< TypeRepr.of[scala.collection.Iterator[_]] ||
          tpe <:< TypeRepr.of[scala.Array[_]] ||
          tpe <:< TypeRepr.of[java.util.Collection[_]] ||
          tpe <:< TypeRepr.of[java.util.Map[_, _]]
      ) && (!(tpe =:= TypeRepr.of[Nothing]))
    }
  }

  def finalValidtyCheck(expr: Expr[_], instructions: Instructions)(using Quotes) =
    import quotes.reflect._
    given implicitInstr: Instructions = instructions
    Trees.traverse(expr.asTerm, Symbol.spliceOwner) {
      // If there are code blocks remaining that have arbitrary zio values it means that
      // they will be lost because the transformations for block-stmts to map/flatMap chains are already done.
      case Block(stmts, output) =>
        stmts.foreach(Unsupported.Warn.checkUnmooredZio(_))
      case tree @ RunCall(_) =>
        Unsupported.Error.withTree(tree, Messages.RunRemainingAfterTransformer)
    }

  def validateBlocksIn(using Quotes)(expr: Expr[_], instructions: Instructions): Unit =
    import quotes.reflect._
    given implicitInstr: Instructions = instructions
    validateBlocksTree(expr.asTerm)

  private def validateRunClause(using qctx: Quotes, instructions: Instructions)(expr: quotes.reflect.Tree): Unit =
    import quotes.reflect._
    Trees.traverse(expr, Symbol.spliceOwner) {
      // Cannot have nested runs:
      case tree @ RunCall(_) =>
        Unsupported.Error.withTree(tree, Messages.RunInRunError)

      // Assignment in an run allowed by not recommenteded
      case asi: Assign if (instructions.verify == Verify.Strict) =>
        Unsupported.Warn.withTree(asi, Messages.RunAssignmentNotRecommended)
    }

  // TODO this way of traversing the tree is error prone not not very efficient. Re-write this
  // using the TreeTraverser directly and make `traverse` private.
  private def validateBlocksTree(using qctx: Quotes, instructions: Instructions)(inputTree: quotes.reflect.Tree): Unit =
    import quotes.reflect._

    val declsErrorMsg =
      instructions.verify match {
        case Verify.Strict  => Messages.DeclarationNotAllowed
        case Verify.Lenient => Messages.DeclarationNotAllowedWithRuns
        // Verify.None should stop "Allowed" from running so this should not be used
        case Verify.None => "No Verification is being done."
      }

    // Unless we are on "silent model," print enough diagnostic information to know why
    // the error is happening.
    def declsError(intro: String, tree: Tree) =
      if (instructions.info == InfoBehavior.Silent)
        s"${intro}. ${declsErrorMsg}"
      else
        s"""|${intro}. ${declsErrorMsg}
            |== Invalid Statement ========
            |${if (instructions.info == InfoBehavior.VerboseTree) Format(Printer.TreeStructure.show(tree)) else Format.Tree(tree)}
            |== Invalid Block ========
            |${if (instructions.info == InfoBehavior.VerboseTree) Format(Printer.TreeStructure.show(inputTree)) else Format.Tree(inputTree)}
            |""".stripMargin

    sealed trait Next
    object Next {
      case object Proceed extends Next
      case class ProceeedSpecific(terms: List[Term]) extends Next
      case object Exit extends Next
    }

    def validate(expr: Tree): Next =
      expr match {
        // if we have transformed the tree before then we can skip validation of the contents
        // because the whole block is treated an an effective unit
        case Seal('{ deferred[r, e, a]($effect) }) =>
          Next.Exit

        case Seal('{ ignore($code) }) =>
          Next.Exit

        case CaseDef(pattern, cond, output) =>
          cond match {
            case None              => Next.Proceed
            case Some(PureTree(_)) => Next.Proceed
            case Some(nonpure) =>
              Unsupported.Error.runUnsupported(nonpure, "Match conditionals are not allow to contain `run`. Move the `run` call out of the match-statement.")
          }

        // should be handled by the tree traverser but put here just in case
        case tree @ RunCall(content) =>
          validateRunClause(content.asTerm)
          // Do not need other validations inside the run-clause
          Next.Exit

        // if verification is in "Lenient mode", allow ClassDefs, DefDefs, and ValDefs so long
        // as there are no 'run' calls inside of them
        case PureTree(_) if (instructions.verify == Verify.Lenient) =>
          Next.Exit

        // evaluate standard terms i.e. that do not have any kind of special handing such as being side of run(...) calls etc...
        case term: Term =>
          validateTerm(term)

        // Do not allow declarations inside of defer blocks. Sometimes when it's a class-constructor
        // etc... there's no Flags.Synthetic but we still need to treat the symbol as though it is synthetic
        // because the user is not explicitly creating it.
        case v: ClassDef if (!v.symbol.flags.is(Flags.Synthetic) && !SymbolOps.isSynthetic(v.symbol)) =>
          Unsupported.Error.withTree(v, declsError(s"Illegal ClassDef (flags: ${v.symbol.flags.show})", v))
        // defdefs are not allowed inside of defer blocks unless they are auto-generated by scala
        // (scala does that when doing implicit-functions etc...)
        case v: DefDef if (!v.symbol.flags.is(Flags.Synthetic) && !SymbolOps.isSynthetic(v.symbol)) =>
          Unsupported.Error.withTree(v, declsError(s"Function Definitions are not allowed. (flags: ${v.symbol.flags.show})", v))
        case v: ValDef if (v.symbol.flags.is(Flags.Mutable) && !v.symbol.flags.is(Flags.Synthetic) && !SymbolOps.isSynthetic(v.symbol)) =>
          Unsupported.Error.withTree(v, declsError(s"Mutable Variable Definitions are not allowed. (flags: ${v.symbol.flags.show})", v))
        case v: ValDef if (v.symbol.flags.is(Flags.Lazy) && !v.symbol.flags.is(Flags.Synthetic) && !SymbolOps.isSynthetic(v.symbol)) =>
          Unsupported.Error.withTree(v, declsError(s"Lazy Variable Definitions are not allowed. (flags: ${v.symbol.flags.show})", v))
        // do not allow implicit inside of defer clauses (if they are not parameters)
        case v: ValDef if ((v.symbol.flags.is(Flags.Given) || v.symbol.flags.is(Flags.Implicit)) && !v.symbol.flags.is(Flags.Param) && !v.symbol.flags.is(Flags.Synthetic) && !SymbolOps.isSynthetic(v.symbol)) =>
          Unsupported.Error.withTree(v, declsError(Messages.ImplicitsNotAllowed, v))
        // otherwise ignore, the tree traversal will continue
        case _ =>
          Next.Proceed
      }
    end validate

    def validateTerm(expr: Term): Next =
      expr match {
        // special error for assignment
        case asi: Assign =>
          Unsupported.Error.withTree(asi, Messages.AssignmentNotAllowed)

        case _ if (FromMutablePackage.check(expr.tpe)) =>
          Unsupported.Error.withTree(expr, Messages.MutableCollectionDetected)

        // All the kinds of valid things a Term can be in defer blocks
        // Originally taken from TreeMap.transformTerm in Quotes.scala
        case v @ Ident(name) =>
          // Not throw error for lazy things here because modules & functions also have the lazy flag. Would need
          // to be sure to allow all of those constructs with other flags.
          if (v.symbol.flags.is(Flags.Mutable) && !v.symbol.flags.is(Flags.Synthetic) && !SymbolOps.isSynthetic(v.symbol))
            Unsupported.Error.withTree(v, Messages.MutableAndLazyVariablesNotAllowed + s"\n=========\n(flags: ${v.symbol.flags.show})")
          Next.Proceed
        case Select(qualifier, name) => Next.Proceed
        case This(qual)              => Next.Proceed
        case Super(qual, mix)        => Next.Proceed

        case applyNode @ Apply(fun, args) =>
          // If we find implicit arguments in the Apply do not check them. It is very important to skip checking of implicit arguments
          // because they do not interfere with code correctness (implicit defs are forbidden because they are defs, implicit vals
          // are allowed because they have to be eager anyway. However, there are situations synthesized code pulugs in implicit things
          // that have defs inside e.g. the LightTypeTag for many zio constructs). In such cases bogus messages of "Illegal DefDef (flags: Flags.Method)."
          // will happen in user code on terms `def tag: LightTypeTag = tag0.tag` that user haven't even seen! (because they are auto-synthesized by zio code).
          // Unfortuately this kind erratic behavior is very difficult to actually reproduce in a test because in only appears when the compiler is confused.
          val nonImplicitArgs = ImplicitArgs.fromFunctionMarked(applyNode).filter { case (_, stat) => !stat.isImplicit }.map(_._1)
          Next.ProceeedSpecific(fun +: nonImplicitArgs)

        case TypeApply(fun, args)   => Next.Proceed
        case Literal(const)         => Next.Proceed
        case New(tpt)               => Next.Proceed
        case Typed(expr, tpt)       => Next.Proceed
        case Block(stats, expr)     => Next.Proceed
        case If(cond, thenp, elsep) => Next.Proceed
        // Anonymous functions run from things inside of Defer can have these
        case Closure(meth, tpt) if (meth.symbol.flags.is(Flags.Synthetic)) =>
          Next.Proceed
        case Match(selector, cases)             => Next.Proceed
        case Return(expr, from)                 => Next.Proceed
        case While(cond, body)                  => Next.Proceed
        case Try(block, cases, finalizer)       => Next.Proceed
        case Inlined(call, bindings, expansion) => Next.Proceed
        case SummonFrom(cases)                  => Next.Proceed
        case v: Repeated                        => Next.Proceed

        case otherTree =>
          Unsupported.Error.runUnsupported(otherTree)
      }
    end validateTerm

    (new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        val nextStep = validate(tree)
        nextStep match {
          // The user can tell use not to check all further elements, just some of them.
          case Next.ProceeedSpecific(specificTerms) =>
            specificTerms.foreach(specificTerm => traverseTree(specificTerm)(owner))
          // This is the most typical, delve into the tree that the user has specified
          case Next.Proceed => super.traverseTree(tree)(owner)
          case Next.Exit    =>
        }
      }
    ).traverseTree(inputTree)(Symbol.spliceOwner)
  end validateBlocksTree
}
