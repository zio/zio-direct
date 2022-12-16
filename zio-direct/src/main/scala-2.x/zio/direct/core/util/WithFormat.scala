package zio.direct.core.util

// import org.scalafmt.config.ScalafmtRunner
import zio.direct.core.metaprog.MacroBase

trait WithFormat extends MacroBase {
  import c.universe._

  object Format {
    sealed trait Mode {
      def showDetails: ShowDetails
    }
    object Mode {
      case class ScalaFmt(showDetails: ShowDetails = ShowDetails.Compact) extends Mode
      case class None(showDetails: ShowDetails = ShowDetails.Compact) extends Mode
    }

    // import org.scalafmt.interfaces.Scalafmt
    // import org.scalafmt.cli.Scalafmt210
    object TypeOf {
      def apply[T: TypeTag] =
        Format.Type(typeOf[T])
    }

    object Type {
      // since we need a qctx to be able to actually pass in a TypeRepr element, it's not possible
      // (unless lampepfl/dotty#10689 is resolved) to create a global module that does TypeRepr formatting. This is a bit
      // of a hacky way around that that just requires the element to be an inner class of a Quotes instance
      // and the casts it to the specific Quotes insance. Should reconsider this when lampepfl/dotty#10689 is fixed.
      def apply(typeRepr: c.universe.Type) =
        show(typeRepr)
    }

    object Term {
      def apply(term: Tree, mode: Mode = Mode.ScalaFmt()) =
        printShortCode(term, mode)
    }

    object Tree {
      def apply(term: Tree, mode: Mode = Mode.ScalaFmt()) =
        printShortCode(term, mode)
    }

    object TermRaw {
      def apply(term: Tree) =
        showRaw(term)
    }

    // Originally it did c.typecheck(code) but I don't want
    // typechecking to be done in the printer, it should either be done before (in the Transformer?)
    // or after. I don't think we can have an assumption that all code going to the pretty-printer
    // (at least in Scala 2 macros) will typecheck.
    private def printShortCode(code: Tree, mode: Mode): String = {
      val printedCode = zio.direct.core.util.CleanCodePrinter.show(c)(code, mode.showDetails)
      // mode match {
      //   case Mode.DottyColor(details) =>
      //     SourceCode.showTree(code)(details, SyntaxHighlight.ANSI, false)
      //   case Mode.DottyPlain(details) =>
      //     SourceCode.showTree(code)(details, SyntaxHighlight.plain, false)
      //   case Mode.ScalaFmt(details) =>
      //     SourceCode.showTree(code)(details, SyntaxHighlight.plain, false)
      //       .map(code => Format(code))
      //   case Mode.None(details) =>
      //     SourceCode.showTree(code)(details, SyntaxHighlight.plain, false)
      // }
      // printedCode match {
      //   case Success(v) => v
      //   case Failure(e) => s"""CannotPrintSourceCode(${e.getMessage()})"""
      // }
      printedCode
    }

    def apply(code: String, formatCode: Boolean = true, showErrorTrace: Boolean = false) = {
      val encosedCode =
        s"""|object DummyEnclosure {
            |  ${code}
            |}""".stripMargin

      // NOTE: Very ineffifient way to get rid of DummyEnclosure on large blocks of code
      //       use only for debugging purposes!
      def unEnclose(enclosedCode: String) = {
        val lines =
          enclosedCode
            .replaceFirst("^object DummyEnclosure[\\s\\t]*", "")
            .replaceFirst("[\\s\\t]*\\{[\\s\\t]*", "")
            .reverse
            .replaceFirst("\\}", "")
            .reverse
            .split("\n")
        val linesTrimmedFirst = if (lines.head == "") lines.drop(1) else lines
        // if there was a \n} on the last line, remove the }
        val linesTrimmedLast = if (linesTrimmedFirst.last == "") linesTrimmedFirst.dropRight(1) else linesTrimmedFirst
        // then if all lines had at least one indent i.e. "  " remove that
        if (linesTrimmedLast.forall(line => line.startsWith("  ")))
          linesTrimmedLast.map(line => line.replaceFirst("  ", "")).mkString("\n")
        else
          linesTrimmedLast.mkString("\n")
      }

      val formatted =
        scala.util.Try {
          if (formatCode) {
            ScalafmtFormat(
              // Various other cleanup needed to make the formatter happy
              encosedCode
                .replace("_*", "_")
                .replace("_==", "==")
                .replace("_!=", "!="),
              showErrorTrace
            )
          } else {
            encosedCode
          }
        }.getOrElse {
          println("====== WARNING: Scalafmt Not Detected ====")
          encosedCode
        }

      unEnclose(formatted)
    }
  }

}
