package zio.direct.core.util

import scala.util.{Try, Success, Failure}
import scala.quoted._
import io.getquill.util.ScalafmtFormat
import zio.direct.core.metaprog.Trees
import zio.direct.core.metaprog.Extractors.Seal
import scala.meta.internal.semanticdb.Scala
import org.scalafmt.config.ScalafmtRunner

object Format {
  sealed trait Mode
  object Mode {
    case class DottyColor(showDetails: ShowDetails = ShowDetails.Compact) extends Mode
    case class DottyPlain(showDetails: ShowDetails = ShowDetails.Compact) extends Mode
    case class ScalaFmt(showDetails: ShowDetails = ShowDetails.Compact) extends Mode
    case class None(showDetails: ShowDetails = ShowDetails.Compact) extends Mode
  }

  // import org.scalafmt.interfaces.Scalafmt
  // import org.scalafmt.cli.Scalafmt210
  object TypeOf {
    def apply[T: Type](using Quotes) =
      import quotes.reflect._
      Format.Type(summon[Type[T]])
  }

  object TypeRepr {
    // since we need a qctx to be able to actually pass in a TypeRepr element, it's not possible
    // (unless lampepfl/dotty#10689 is resolved) to create a global module that does TypeRepr formatting. This is a bit
    // of a hacky way around that that just requires the element to be an inner class of a Quotes instance
    // and the casts it to the specific Quotes insance. Should reconsider this when lampepfl/dotty#10689 is fixed.
    def apply(typeRepr: Quotes#reflectModule#TypeRepr)(using qctx: Quotes) =
      import qctx.reflect._
      Printer.TypeReprShortCode.show(typeRepr.asInstanceOf[qctx.reflect.TypeRepr])
  }

  object Term:
    def apply(term: Quotes#reflectModule#Term, mode: Mode = Mode.ScalaFmt())(using qctx: Quotes) =
      import qctx.reflect._
      printShortCode(using qctx)(term.asInstanceOf[qctx.reflect.Term], mode)

  object Tree:
    def apply(term: Quotes#reflectModule#Tree, mode: Mode = Mode.ScalaFmt())(using qctx: Quotes) =
      import qctx.reflect._
      printShortCode(using qctx)(term.asInstanceOf[qctx.reflect.Tree], mode)

  object TermRaw:
    def apply(term: Quotes#reflectModule#Term)(using qctx: Quotes) =
      import qctx.reflect._
      Printer.TreeStructure.show(term.asInstanceOf[qctx.reflect.Term])

  /** Same as TypeRepr but also widens the type since frequently types are singleton i.e. 'person.name' has the type 'name' as opposed to String */
  object TypeReprW {
    def apply(typeRepr: Quotes#reflectModule#TypeRepr)(using qctx: Quotes) =
      import qctx.reflect._
      Printer.TypeReprShortCode.show(typeRepr.asInstanceOf[qctx.reflect.TypeRepr].widen)
  }

  object Type {
    def apply(tpe: scala.quoted.Type[_])(using Quotes) =
      import quotes.reflect._
      tpe match
        case '[tt] => Printer.TypeReprShortCode.show(quotes.reflect.TypeRepr.of[tt])
        case _     => tpe
  }

  object Expr {
    def apply(expr: Expr[_], mode: Mode = Mode.ScalaFmt(), showErrorTrace: Boolean = false)(using q: Quotes) =
      import quotes.reflect._
      Format(printShortCode(using q)(expr.asTerm, mode), showErrorTrace)
  }

  private def printShortCode(using Quotes)(code: quotes.reflect.Tree, mode: Mode): String =
    import quotes.reflect._
    val printedCode =
      mode match {
        case Mode.DottyColor(details) =>
          SourceCode.showTree(code)(details, SyntaxHighlight.ANSI, false)
        case Mode.DottyPlain(details) =>
          SourceCode.showTree(code)(details, SyntaxHighlight.plain, false)
        case Mode.ScalaFmt(details) =>
          SourceCode.showTree(code)(details, SyntaxHighlight.plain, false)
            .map(code => Format(code))
        case Mode.None(details) =>
          SourceCode.showTree(code)(details, SyntaxHighlight.plain, false)
      }
    printedCode match {
      case Success(v) => v
      case Failure(e) => s"""CannotPrintSourceCode(${e.getMessage()})"""
    }

  private def printShortCode(using q: Quotes)(expr: Expr[_], mode: Mode): String =
    import quotes.reflect._
    printShortCode(using q)(expr.asTerm, mode)

  def apply(code: String, formatCode: Boolean = true, showErrorTrace: Boolean = true) = {
    val encosedCode =
      s"""|object DummyEnclosure {
            |  ${code}
            |}""".stripMargin

    // NOTE: Very ineffifient way to get rid of DummyEnclosure on large blocks of code
    //       use only for debugging purposes!
    def unEnclose(enclosedCode: String) =
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

    val formatted =
      Try {
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

  object ScalafmtFormat {
    import io.getquill.util.ThrowableOps._
    import org.scalafmt.config.ScalafmtConfig
    import org.scalafmt.{Formatted, Scalafmt}

    def apply(code: String, showErrorTrace: Boolean = false): String = {
      val style = ScalafmtConfig(runner = ScalafmtRunner(dialect = ScalafmtRunner.Dialect.scala3))
      Scalafmt.format(code, style, Set.empty, "<input>") match {
        case Formatted.Success(formattedCode) =>
          formattedCode
        case Formatted.Failure(e) =>
          if (showErrorTrace)
            println(
              s"""===== Failed to format the code ====
                |$code
                |---
                |${e.stackTraceToString}.
                |""".stripMargin
            )
          code
      }
    }
  }
}
