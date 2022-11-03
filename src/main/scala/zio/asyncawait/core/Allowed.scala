package zio.asyncawait.core

import scala.quoted._
import zio.asyncawait.core.util.Format
import zio.asyncawait.core.metaprog.Trees
import zio.asyncawait.core.metaprog.Extractors._
import zio.asyncawait.await
import zio.asyncawait.core.util.PureTree
import zio.asyncawait.core.util.UnsupportedError

object Allowed {

  def validateBlocksIn(using Quotes)(expr: Expr[_]): Unit =
    import quotes.reflect._
    validateBlocksTree(expr.asTerm)

  private def validateBlocksTree(using Quotes)(expr: quotes.reflect.Tree): Unit =
    import quotes.reflect._
    Trees.traverse(expr, Symbol.spliceOwner) {
      case Select(term, _) =>
        validateBlocksTree(term)
      // Generally speaking, async/awaits can be used both inside of functions and function paramters
      // with the exception that a function that does await cannot also have await paremters. This
      // latter condition is verified in TransformDefs
      case Apply(term, args) =>
        validateBlocksTree(term)

      // TODO Should we diallow assignment anywhere inside an async block?
      //      if so, that conditional should be added here.

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
      case Match(input, caseDefs) =>
        validateBlocksTree(input)
        caseDefs.foreach {
          case CaseDef(pattern, cond, output) =>
            cond match {
              case None =>
              case Some(PureTree(_)) =>
              case Some(nonpure) =>
                UnsupportedError.throwIt(nonpure, "Match conditionals are not allow to contain `await`. Move the `await` call out of the match-statement.")
            }
            validateBlocksTree(output)
        }
      case Block(stmts, ret) =>
        (stmts :+ ret).foreach(validateBlocksTree(_))
      // otherwise it has an await clause and with an unsupported construct
      case Seal('{ await[t]($content) }) =>
      case otherTree =>
        UnsupportedError.throwIt(otherTree)

      // TODO custom warning if any mutable structures are being used inside async blocks
      // TODO custom warning for assignment
      // TODO custom warning for awaits in defs (and tip for how to fix)
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
          UnsupportedError.throwIt(tree)
      }
    }

    private def checkTerms(using Quotes)(trees: List[quotes.reflect.Term]): Boolean =
      trees.forall(x => checkAllowed(x))
  }
}
