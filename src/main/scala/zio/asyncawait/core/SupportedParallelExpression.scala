package zio.asyncawait.core

import scala.quoted._

object SupportedParallelExpression {
  def unapply(using Quotes)(term: quotes.reflect.Term): Boolean =
    transformTerm(term)

  private def transformTerm(using Quotes)(tree: quotes.reflect.Term): Boolean = {
    import quotes.reflect._
    tree match {
      case Ident(name) =>
        true
      case Select(qualifier, name) =>
        transformTerm(qualifier)
      case This(qual) =>
        true
      case Super(qual, mix) =>
        transformTerm(qual)
      case Apply(fun, args) =>
        transformTerms(args)
      case TypeApply(fun, args) =>
        transformTerm(fun)
      case Literal(const) =>
        true
      case New(tpt) =>
        true
      case Typed(expr, tpt) =>
        transformTerm(expr)
      case tree: NamedArg =>
        transformTerm(tree.value)
      case Repeated(elems, elemtpt) =>
        transformTerms(elems)
      case Inlined(call, bindings, expansion) =>
        transformTerm(expansion)
      case _ =>
        report.errorAndAbort(
          s"""Detected an `await` call inside of an unsupported structure:
             |${tree.show(using Printer.TreeShortCode)}
             |Move the `await` call outside of this structure in order to use it.
             |For example, change this:
             |  val v = somethingUnsupported(await(x))
             |To this:
             |  val a = await(x)
             |  val v = somethingUnsupported(a)
          """.stripMargin

        )
    }
  }

  private def transformTerms(using Quotes)(trees: List[quotes.reflect.Term]): Boolean =
    trees.forall(x => transformTerm(x))
}