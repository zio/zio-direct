package zio.direct.core.util.debug

import scala.reflect.macros.whitebox.{Context => MacroContext}
import zio.direct.core.util.WithFormat

class PrintMacMacro(val c: MacroContext) extends WithFormat {
  import c.universe._

  def apply(value: Tree): Tree = {
    println(
      "================= Printing Tree =================\n" +
        show(value)
    )
    q"()"
  }

  def detail(value: Tree): Tree = {
    println(
      "================= Printing Tree =================\n" +
        show(value) + "\n" +
        "================= Printing Tree =================\n" +
        Format(showRaw(value))
    )
    q"()"
  }

}
