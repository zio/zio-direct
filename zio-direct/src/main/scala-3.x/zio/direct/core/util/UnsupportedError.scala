package zio.direct.core.util

import scala.quoted._
import zio.direct.core.util.IndentExt._
import zio.ZIO

object Unsupported {
  private sealed trait Msg {
    def render: String
  }
  private object Msg {
    case class Simple(msg: String) extends Msg {
      def render = msg
    }

    def AwaitUnsupportedTree(using Quotes)(tree: quotes.reflect.Tree, additionalMsg: String = "", example: String = Messages.MoveAwaitOut) =
      AwaitUnsupported(Format.Tree(tree), additionalMsg, example)

    case class AwaitUnsupported(unsupportedConstruct: String, additionalMsg: String = "", example: String = Messages.MoveAwaitOut) extends Msg {
      def render =
        s"""|Detected an `await` call inside of an unsupported structure:
            |${unsupportedConstruct}
            |""".stripMargin
          + lineIfAnyDefined("============")(additionalMsg, example)
          + lineIfDefined(additionalMsg)
          + lineIfDefined(example)
    }

    private def lineIfAnyDefined(strToShow: String)(ifDefined: String*) =
      if (ifDefined.exists(_.trim != ""))
        strToShow + "\n"
      else
        ""

    private def lineIfDefined(str: String) =
      if (str.trim != "") str + "\n"
      else ""
  }

  object Error {
    def awaitUnsupported(using Quotes)(tree: quotes.reflect.Tree, additionalMessage: String = "") =
      import quotes.reflect._
      val text = Msg.AwaitUnsupportedTree(tree, additionalMessage).render
      tree match
        case t: Term if (t.isExpr) => report.errorAndAbort(text, t.asExpr)
        case _                     => report.errorAndAbort(text, tree.pos)

    def withTree(using Quotes)(tree: quotes.reflect.Tree, message: String) =
      import quotes.reflect._
      val text =
        s"""|${message}
            |=========
            |${Format.Tree(tree)}
            |""".stripMargin
      tree match
        case t: Term if (t.isExpr) => report.errorAndAbort(text, t.asExpr)
        case _                     => report.errorAndAbort(text, tree.pos)
  }

  object Warn {
    def withTree(using Quotes)(tree: quotes.reflect.Tree, message: String) = {
      import quotes.reflect._
      val text =
        s"""|${message}
            |=========
            |${Format.Tree(tree)}
            |""".stripMargin
      tree match
        case t: Term if (t.isExpr) => report.warning(text, t.asExpr)
        case _                     => report.warning(text, tree.pos)
    }

    def checkUnmooredZio(using Quotes)(tree: quotes.reflect.Tree) = {
      import quotes.reflect._
      tree match
        case term: Term =>
          term.tpe.asType match
            case '[ZIO[r, e, a]] if (!(term.tpe =:= TypeRepr.of[Nothing])) =>
              report.warning(
                s"Found a ZIO term that is not being awaited (type: ${Format.TypeRepr(term.tpe)}). Non-awaited ZIO terms inside of `{ ... }` blocks will never be executed i.e. they will be discarded. " +
                  s"To execute this term add `.run` at the end or wrap it into an `run(...)` statement." +
                  s"\n========\n" +
                  Format.Term(term),
                term.asExpr
              )
            case _ =>
        case _ =>
    }

  }
}
