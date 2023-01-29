package zio.direct.core.util

import scala.quoted._
import zio.direct.core.util.IndentExt._
import zio.ZIO
import zio.direct.core.metaprog.Instructions
import zio.direct.core.metaprog.InfoBehavior
import zio.direct.core.metaprog.WithIR
import zio.direct.core.metaprog.WithZioType

trait WithUnsupported {
  self: WithZioType =>

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  object Unsupported {
    private sealed trait Msg {
      def render: String
    }
    private object Msg {
      case class Simple(msg: String) extends Msg {
        def render = msg
      }

      def runUnsupportedTree(using instr: Instructions)(tree: quotes.reflect.Tree, additionalMsg: String = "", example: String = Messages.MoveOutOfDefer) = {
        val text =
          s"""|Detected an `run` call inside of an unsupported structure:
            |${Format.Tree(tree)}
            |""".stripMargin
            + lineIfAnyDefined("============")(additionalMsg, example)
            + lineIfDefined(additionalMsg)
            + lineIfDefined(example)

        lazy val extMessage =
          s"""|========= (Detail)
            |${Format(Printer.TreeStructure.show(tree))}
            |""".stripMargin

        if (instr.info.showReconstructedTree)
          text + extMessage
        else
          text
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
      def runUnsupported(using Instructions)(tree: quotes.reflect.Tree, additionalMessage: String = "")(using instr: Instructions) =
        import quotes.reflect._
        val text = Msg.runUnsupportedTree(tree, additionalMessage)
        tree match
          case t: Term if (t.isExpr) => report.errorAndAbort(text, t.asExpr)
          case _                     => report.errorAndAbort(text, tree.pos)

      def withTree(using instr: Instructions)(tree: quotes.reflect.Tree, message: String): Nothing =
        withTree(tree, message, instr.info)

      def withTree(tree: quotes.reflect.Tree, message: String, info: InfoBehavior): Nothing =
        import quotes.reflect._
        val text =
          s"""|${message}
            |=========
            |${Format.Tree(tree)}
            |""".stripMargin

        lazy val extMessage =
          s"""|========= (Detail)
            |${Format(Printer.TreeStructure.show(tree))}
            |""".stripMargin

        val textOuput =
          if (info.showReconstructedTree)
            text + extMessage
          else
            text

        tree match
          case t: Term if (t.isExpr) => report.errorAndAbort(textOuput, t.asExpr)
          case _                     => report.errorAndAbort(textOuput, tree.pos)
    }

    object Warn {
      def withTree(tree: quotes.reflect.Tree, message: String) = {
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

      def checkUnmooredZio(effectType: ZioEffectType)(tree: quotes.reflect.Tree) = {
        import quotes.reflect._
        tree match
          case term: Term =>
            term.tpe match
              case effectType(_) if (!(term.tpe =:= TypeRepr.of[Nothing])) =>
                report.warning(
                  s"""|Found a ZIO-effect term that will not be executed (type: ${Format.TypeRepr(term.tpe)})
                    |since it has no `.run` (or wrapped in a `run(...)` block). Non-executed ZIO terms
                    |inside of `defer { ... }` blocks will never be executed i.e. they will be discarded.
                    |To execute this term add `.run` at the end or wrap it into an `run(...)` statement.
                    |========
                    |${Format.Term(term)}
                    |""".stripMargin,
                  term.asExpr
                )
              case _ =>
          case _ =>
      }

    }
  }
}
