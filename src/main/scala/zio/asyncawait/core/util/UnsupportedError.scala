package zio.asyncawait.core.util

import scala.quoted._
import zio.asyncawait.core.util.IndentExt._

sealed trait Msg {
  def render: String
}
object Msg {
  case class Simple(msg: String) extends Msg {
    def render = msg
  }

  def UnsuppTree(using Quotes)(tree: quotes.reflect.Tree, additionalMsg: String = "", example: Example = defaultExample) =
    Unsupp(Format.Tree(tree), additionalMsg, example)

  case class Unsupp(unsupportedConstruct: String, additionalMsg: String = "", example: Example = defaultExample) extends Msg {
    def render =
      s"""|Detected an `await` call inside of an unsupported structure:
          |${unsupportedConstruct}
          |===========
          |${additionalMsg}
          |${example.text}
          |""".stripMargin
  }
  case class Example(text: String) extends Msg {
    def render = text
  }

  val defaultExample = Example(defaultExampleMsg)
  private val defaultExampleMsg =
    s"""Move the `await` call outside of this structure in order to use it.
      |For example, change this:
      |  val v = somethingUnsupported(await(x))
      |To this:
      |  val a = await(x)
      |  val v = somethingUnsupported(a)
      """.stripMargin
}

object UnsupportedError {
  def throwIt(using Quotes)(tree: quotes.reflect.Tree, additionalMessage: String = "") =
    import quotes.reflect._
    val text = Msg.UnsuppTree(tree, additionalMessage).render
    tree match
      case t: Term if (t.isExpr) => report.errorAndAbort(text, t.asExpr)
      case _ => report.errorAndAbort(text)

  def throwItMsg(using Quotes)(tree: quotes.reflect.Tree, makeMsg: quotes.reflect.Tree => Msg = Msg.UnsuppTree(_)) =
    import quotes.reflect._
    val text = makeMsg(tree).render
    tree match
      case t: Term if (t.isExpr) => report.errorAndAbort(text, t.asExpr)
      case _ => report.errorAndAbort(text)
}