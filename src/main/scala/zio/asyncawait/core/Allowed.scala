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
      case TypeApply(term, args) =>
        validateBlocksTree(term)
        args.foreach(validateBlocksTree(_))
      // Ignore things that do not have await clauses inside
      case PureTree(_) =>
      // Don't need to go inside await blocks since things inside there are regular code
      // (i.e. not macro transformed)
      case DefDef(_, _, _, Some(rhs)) => validateBlocksTree(rhs)
      case Seal('{ await[t]($content) }) =>
      // Otherwise, anywhere we see a block, validate the contents of the block and throw an error if needed
      case Match(input, caseDefs) =>
        validateBlocksTree(input)
        caseDefs.foreach(validateBlocksTree(_))
      case Block(stmts, ret) =>
        (stmts :+ ret).foreach(validateBlocksTree(_))
      // otherwise it has an await clause and with an unsupported construct
      case otherTree =>
        UnsupportedError.throwIt(otherTree)
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
