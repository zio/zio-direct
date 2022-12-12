package zio.direct.core.util

import zio.direct.core.metaprog.Instructions
import zio.direct.core.metaprog.InfoBehavior
import zio.direct.core.metaprog.MacroBase

trait WithUnsupported extends WithFormat with MacroBase {
  import c.universe._

  object Unsupported {
    private sealed trait Msg {
      def render: String
    }
    private object Msg {
      case class Simple(msg: String) extends Msg {
        def render = msg
      }

      def runUnsupportedTree(tree: Tree, additionalMsg: String = "", example: String = Messages.MoveOutOfDefer)(implicit instr: Instructions) = {
        val text =
          s"""|Detected an an unsupported structure:
              |${Format.Tree(tree)}
              |""".stripMargin +
            lineIfAnyDefined("============")(additionalMsg, example) +
            lineIfDefined(additionalMsg) +
            lineIfDefined(example)

        lazy val extMessage =
          s"""|========= (Detail)
              |${Format(show(tree))}
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
      def runUnsupported(tree: Tree, additionalMessage: String = "")(implicit instr: Instructions) = {

        val text = Msg.runUnsupportedTree(tree, additionalMessage)
        report.errorAndAbort(text, tree)
      }

      def withTree(tree: Tree, message: String)(implicit instr: Instructions): Nothing =
        withTree(tree, message, instr.info)

      def withTree(tree: Tree, message: String, info: InfoBehavior): Nothing = {

        val text =
          s"""|${message}
              |=========
              |${Format.Tree(tree)}
              |""".stripMargin

        lazy val extMessage =
          s"""|========= (Detail)
              |${Format(show(tree))}
              |""".stripMargin

        val textOuput =
          if (info.showReconstructedTree)
            text + extMessage
          else
            text

        report.errorAndAbort(textOuput, tree)
      }
    }

    object Warn {
      def withTree(tree: Tree, message: String) = {

        val text =
          s"""|${message}
              |=========
              |${Format.Tree(tree)}
              |""".stripMargin
        report.warning(text, tree)
      }

      def checkUnmooredZio(term: Tree) = {

        isZIO(term.tpe) match {
          case true =>
            report.warning(
              s"""|Found a ZIO term that will not be executed (type: ${Format.Type(term.tpe)})
                  |since it has no `.run` (or wrapped in a `run(...)` block). Non-executed ZIO terms
                  |inside of `defer { ... }` blocks will never be executed i.e. they will be discarded.
                  |To execute this term add `.run` at the end or wrap it into an `run(...)` statement.
                  |========
                  |${Format.Term(term)}
                  |""".stripMargin,
              term
            )
          case _ =>
        }
      }

    }
  }
}
