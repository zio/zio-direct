package zio.asyncawait.core.util

import scala.quoted._
import zio.asyncawait.core.util.IndentExt._

object UnsupportedError {
  def throwIt(using Quotes)(tree: quotes.reflect.Tree) =
    import quotes.reflect._
    report.errorAndAbort(
      s"""Detected an `await` call inside of an unsupported structure:
          |${Format.Tree(tree).indent(1)}
          |Move the `await` call outside of this structure in order to use it.
          |For example, change this:
          |  val v = somethingUnsupported(await(x))
          |To this:
          |  val a = await(x)
          |  val v = somethingUnsupported(a)
      """.stripMargin
    )
}