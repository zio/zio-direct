package zio.direct.core.metaprog

import zio.direct.core.util.WithFormat

trait WithUseParser extends MacroBase {
  self: WithFormat =>

  import c.universe._

  object RefineInstructions {
    // takes a Use.with___ tree and parses it
    def fromUseTree(tree: Tree, initial: Instructions): Instructions =
      // go through each case, parse the instructions from the earlier statements,
      // then set them afterward. That means that if there are two potentially
      // contradicting instructions e.g. Use.withLenientCheck.withStrictCheck it will run
      // set the Instructions first based on the 1st one, and then the 2nd one
      tree match {
        case q"$use.withLenientCheck" =>
          val updated = fromUseTree(use, initial)
          updated.copy(verify = Verify.Lenient)

        case q"$use.withParallelEval" =>
          val updated = fromUseTree(use, initial)
          updated.copy(collect = Collect.Parallel)

        case q"$use.withAbstractError" =>
          val updated = fromUseTree(use, initial)
          updated.copy(typeUnion = TypeUnion.LeastUpper)

        // This means we got to the initial "Use" object.
        case q"$pack.Use" =>
          initial

        case q"Use" =>
          initial

        case _ =>
          report.errorAndAbort(s"Invalid use-instruction: `${Format.Tree(tree)}`. This should not be possible.")
      }
  }
}
