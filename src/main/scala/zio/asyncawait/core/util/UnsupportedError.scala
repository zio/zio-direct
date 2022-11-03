package zio.asyncawait.core.util

import scala.quoted._
import zio.asyncawait.core.util.IndentExt._

object UnsupportedError {
  def throwIt(using Quotes)(tree: quotes.reflect.Tree, additionalMessage: String = "") =
    import quotes.reflect._
    val msg =
      s"""Detected an `await` call inside of an unsupported structure:
          |${Format.Tree(tree).indent(1)}
          |${if (additionalMessage != "") additionalMessage + "\n" else ""}
          |Move the `await` call outside of this structure in order to use it.
          |For example, change this:
          |  val v = somethingUnsupported(await(x))
          |To this:
          |  val a = await(x)
          |  val v = somethingUnsupported(a)
      """.stripMargin

    tree match
      case t: Term if (t.isExpr) => report.errorAndAbort(msg, t.asExpr)
      case _ => report.errorAndAbort(msg)
}