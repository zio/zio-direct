package zio.asyncawait.core

import scala.quoted._
import zio.asyncawait.core.util.Format
import zio.asyncawait.core.metaprog.Trees
import zio.asyncawait.core.metaprog.Extractors._
import zio.asyncawait.await
import zio.asyncawait.core.util.PureTree
import zio.asyncawait.core.util.Unsupported
import zio.asyncawait.core.util.Examples

object Allowed {

  def validateBlocksIn(using Quotes)(expr: Expr[_]): Unit =
    import quotes.reflect._
    validateBlocksTree(expr.asTerm)

  private def validateAwaitClause(using Quotes)(expr: quotes.reflect.Tree): Unit =
    import quotes.reflect._
    Trees.traverse(expr, Symbol.spliceOwner) {
      // Cannot have nested awaits:
      case tree @ Seal('{ await[r, e, a]($content) }) =>
        Unsupported.Error.withTree(tree, Examples.AwaitInAwaitError)

      // Assignment in an await allowed by not recommenteded
      case asi: Assign =>
        Unsupported.Warn.withTree(asi, Examples.AwaitAssignmentNotRecommended)
    }

  // TODO this way of traversing the tree is error prone not not very efficient. Re-write this
  // using the TreeTraverser directly and make `traverse` private.
  private def validateBlocksTree(using Quotes)(expr: quotes.reflect.Tree): Unit =
    import quotes.reflect._
    Trees.traverse(expr, Symbol.spliceOwner) {
      case tree @ Seal('{ zio.asyncawait.await[r, e, a]($content) }) =>
        validateAwaitClause(content.asTerm)

      case Select(term, _) =>
        validateBlocksTree(term)
      // Generally speaking, async/awaits can be used both inside of functions and function paramters
      // with the exception that a function that does await cannot also have await paremters. This
      // latter condition is verified in TransformDefs
      case Apply(term, args) =>
        validateBlocksTree(term)
        args.foreach(validateBlocksTree(_))

      case asi: Assign =>
        Unsupported.Error.withTree(asi, Examples.AssignmentNotAllowed)

      case TypeApply(term, args) =>
        validateBlocksTree(term)
        args.foreach(validateBlocksTree(_))
      // Ignore things that do not have await clauses inside
      case PureTree(_) =>

      // TODO Allow awaits in the body of ifs, whiles, fors, and do-whiles but not in their conditions
      case ValDef(_, _, rhsOpt) =>
        rhsOpt match
          case None =>
          case Some(rhs) => validateBlocksTree(rhs)
      // DefDef with Await in right-hand-side not allowed anymore
      //case DefDef(_, _, _, Some(rhs)) => validateBlocksTree(rhs)
      // Otherwise, anywhere we see a block, validate the contents of the block and throw an error if needed

      case If(cond, ifTrue, ifFalse) =>
        validateBlocksTree(cond)
        validateBlocksTree(ifTrue)
        validateBlocksTree(ifFalse)

      case While(cond, body) =>
        validateBlocksTree(cond)
        validateBlocksTree(body)

      case Try(tryBlock, caseDefs, finallyBlock) =>
        validateBlocksTree(tryBlock)
        caseDefs.foreach(validateCaseDef(_))
        finallyBlock match {
          case Some(value) => validateBlocksTree(value)
          case None =>
        }

      case Match(input, caseDefs) =>
        validateBlocksTree(input)
        caseDefs.foreach(validateCaseDef(_))

      case Block(stmts, ret) =>
        (stmts :+ ret).foreach(validateBlocksTree(_))

      case Typed(tree, _) =>
        validateBlocksTree(tree)

      case otherTree =>
        Unsupported.Error.awaitUnsupported(otherTree)

      // TODO custom warning if any mutable structures are being used inside async blocks
      // TODO custom warning for assignment
      // TODO custom warning for awaits in defs (and tip for how to fix)
    }

  private def validateCaseDef(using Quotes)(caseDef: quotes.reflect.CaseDef) = {
    import quotes.reflect._
    val CaseDef(pattern, cond, output) = caseDef
    cond match {
      case None =>
      case Some(PureTree(_)) =>
      case Some(nonpure) =>
        Unsupported.Error.awaitUnsupported(nonpure, "Match conditionals are not allow to contain `await`. Move the `await` call out of the match-statement.")
    }
    validateBlocksTree(output)
  }

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
