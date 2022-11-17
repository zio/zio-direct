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
import zio.direct.deferred

object Allowed {

  def finalValidtyCheck(expr: Expr[_])(using Quotes) =
    import quotes.reflect._
    Trees.traverse(expr.asTerm, Symbol.spliceOwner) {
      case tree @ RunCall(_) =>
        Unsupported.Error.withTree(
          tree,
          s"""|${Messages.AwaitInAwaitError}
              |=======
              |${Format.Tree(tree)}
              |""".stripMargin
        )
    }

  def validateBlocksIn(using Quotes)(expr: Expr[_], instructions: Instructions): Unit =
    import quotes.reflect._
    validateBlocksTree(expr.asTerm, instructions)

  private def validateAwaitClause(using Quotes)(expr: quotes.reflect.Tree, instructions: Instructions): Unit =
    import quotes.reflect._
    Trees.traverse(expr, Symbol.spliceOwner) {
      // Cannot have nested awaits:
      case tree @ RunCall(_) =>
        Unsupported.Error.withTree(tree, Messages.AwaitInAwaitError)

      // Assignment in an await allowed by not recommenteded
      case asi: Assign if (instructions.verify == Verify.Strict) =>
        Unsupported.Warn.withTree(asi, Messages.AwaitAssignmentNotRecommended)
    }

  // TODO this way of traversing the tree is error prone not not very efficient. Re-write this
  // using the TreeTraverser directly and make `traverse` private.
  private def validateBlocksTree(using Quotes)(inputTree: quotes.reflect.Tree, instructions: Instructions): Unit =
    import quotes.reflect._

    val declsErrorMsg =
      instructions.verify match {
        case Verify.Strict  => Messages.DeclarationNotAllowed
        case Verify.Lenient => Messages.DeclarationNotAllowedWithAwaits
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
      case object Exit extends Next
    }

    def validate(expr: Tree): Next =
      expr match {
        case CaseDef(pattern, cond, output) =>
          cond match {
            case None              => Next.Proceed
            case Some(PureTree(_)) => Next.Proceed
            case Some(nonpure) =>
              Unsupported.Error.awaitUnsupported(nonpure, "Match conditionals are not allow to contain `await`. Move the `await` call out of the match-statement.")
          }

        case term: Term =>
          validateTerm(term)

        // if verification is in "Lenient mode", allow ClassDefs, DefDefs, and ValDefs so long
        // as there are no 'await' calls inside of them
        case PureTree(_) if (instructions.verify == Verify.Lenient) =>
          Next.Exit

        // Do not allow declarations inside of defer blocks. Sometimes when it's a class-constructor
        // etc... there's no Flags.Synthetic but we still need to treat the symbol as though it is synthetic
        // because the user is not explicitly creating it.
        case v: ClassDef if (!v.symbol.flags.is(Flags.Synthetic) && !SymbolOps.isSynthetic(v.symbol)) =>
          Unsupported.Error.withTree(v, declsError(s"Illegal ClassDef (flags: ${v.symbol.flags.show})", v))
        // defdefs are not allowed inside of defer blocks unless they are auto-generated by scala
        // (scala does that when doing implicit-functions etc...)
        case v: DefDef if (!v.symbol.flags.is(Flags.Synthetic) && !SymbolOps.isSynthetic(v.symbol)) =>
          Unsupported.Error.withTree(v, declsError(s"Illegal DefDef (flags: ${v.symbol.flags.show})", v))
        case v: ValDef if (v.symbol.flags.is(Flags.Mutable) && !v.symbol.flags.is(Flags.Synthetic) && !SymbolOps.isSynthetic(v.symbol)) =>
          Unsupported.Error.withTree(v, declsError(s"Illegal ValDef (flags: ${v.symbol.flags.show})", v))

        // otherwise ignore, the tree traversal will continue
        case _ =>
          Next.Proceed
      }
    end validate

    def validateTerm(expr: Term): Next =
      expr match {
        // if we have transformed the tree before then we can skip validation of the contents
        // because the whole block is treated an an effective unit
        case Seal('{ deferred($effect) }) =>
          Next.Exit

        // should be handled by the tree traverser but put here just in case
        case tree @ RunCall(content) =>
          validateAwaitClause(content.asTerm, instructions)
          // Do not need other validations inside the run-clause
          Next.Exit

        // special error for assignment
        case asi: Assign =>
          Unsupported.Error.withTree(asi, Messages.AssignmentNotAllowed)

        // All the kinds of valid things a Term can be in defer blocks
        // Originally taken from TreeMap.transformTerm in Quotes.scala
        case Ident(name)             => Next.Proceed
        case Select(qualifier, name) => Next.Proceed
        case This(qual)              => Next.Proceed
        case Super(qual, mix)        => Next.Proceed
        case Apply(fun, args)        => Next.Proceed
        case TypeApply(fun, args)    => Next.Proceed
        case Literal(const)          => Next.Proceed
        case New(tpt)                => Next.Proceed
        case Typed(expr, tpt)        => Next.Proceed
        case Block(stats, expr)      => Next.Proceed
        case If(cond, thenp, elsep)  => Next.Proceed
        // Anonymous functions run from things inside of Async can have these
        case Closure(meth, tpt) if (meth.symbol.flags.is(Flags.Synthetic)) =>
          Next.Proceed
        case Match(selector, cases)             => Next.Proceed
        case Return(expr, from)                 => Next.Proceed
        case While(cond, body)                  => Next.Proceed
        case Try(block, cases, finalizer)       => Next.Proceed
        case Inlined(call, bindings, expansion) => Next.Proceed
        case SummonFrom(cases)                  => Next.Proceed

        case otherTree =>
          Unsupported.Error.awaitUnsupported(otherTree)
      }
    end validateTerm

    (new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case tree @ RunCall(content) =>
            validateAwaitClause(content.asTerm, instructions)
          case _ =>
        }
        val nextStep = validate(tree)
        nextStep match {
          case Next.Proceed => super.traverseTree(tree)(owner)
          case Next.Exit    =>
        }
      }
    ).traverseTree(inputTree)(Symbol.spliceOwner)
  end validateBlocksTree

  object ParallelExpression {
    def unapply(using Quotes)(expr: Expr[_]): Boolean =
      import quotes.reflect._
      checkAllowed(expr.asTerm)

    private def checkAllowed(using Quotes)(tree: quotes.reflect.Term): Boolean = {
      import quotes.reflect._
      tree match {
        case Ident(name) =>
          true
        case Select(qualifier, name) =>
          checkAllowed(qualifier)
        case This(qual) =>
          true
        case Super(qual, mix) =>
          checkAllowed(qual)
        case Apply(fun, args) =>
          checkTerms(args)
        case TypeApply(fun, args) =>
          checkAllowed(fun)
        case Literal(const) =>
          true
        case New(tpt) =>
          true
        case Typed(expr, tpt) =>
          checkAllowed(expr)
        case tree: NamedArg =>
          checkAllowed(tree.value)
        case Repeated(elems, elemtpt) =>
          checkTerms(elems)
        case Inlined(call, bindings, expansion) =>
          checkAllowed(expansion)
        case _ =>
          Unsupported.Error.awaitUnsupported(tree)
      }
    }

    private def checkTerms(using Quotes)(trees: List[quotes.reflect.Term]): Boolean =
      trees.forall(x => checkAllowed(x))
  }
}