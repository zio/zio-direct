package zio.direct.core.metaprog

import scala.quoted._
import zio.direct.Use
import zio.direct.core.util.Format

object RefineInstructions {
  // takes a Use.with___ tree and parses it
  def fromUseTree(tree: Expr[Use], initial: Instructions)(using Quotes): Instructions = {
    import quotes.reflect._
    // go through each case, parse the instructions from the earlier statements,
    // then set them afterward. That means that if there are two potentially
    // contradicting instructions e.g. Use.withLenientCheck.withStrictCheck it will run
    // set the Instructions first based on the 1st one, and then the 2nd one
    tree match {
      case '{ ($use: Use).withLenientCheck } =>
        val updated = fromUseTree(use, initial)
        updated.copy(verify = Verify.Lenient)

      case '{ ($use: Use).withNoCheck } =>
        val updated = fromUseTree(use, initial)
        updated.copy(verify = Verify.None)

      case '{ ($use: Use).withParallelEval } =>
        val updated = fromUseTree(use, initial)
        updated.copy(collect = Collect.Parallel)

      case '{ ($use: Use).withAbstractError } =>
        val updated = fromUseTree(use, initial)
        updated.copy(typeUnion = TypeUnion.LeastUpper)

      // This means we got to the initial "Use" object.
      case '{ ($use: Use.type) } =>
        initial

      case _ =>
        report.errorAndAbort(s"Invalid use-instruction: `${Format.Expr(tree)}`. This should not be possible.")
    }
  }
}
