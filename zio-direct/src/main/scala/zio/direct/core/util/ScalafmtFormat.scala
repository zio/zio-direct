package zio.direct.core.util

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.{Formatted, Scalafmt}
import zio.direct.core.util.ThrowableOps._

object ScalafmtFormat {

  def apply(code: String, showErrorTrace: Boolean = false): String = {
    val style = ScalafmtConfig() // runner = ScalafmtRunner(dialect = ScalafmtRunner.Dialect.scala213)
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
