package zio.direct.core.util.debug

import scala.language.experimental.macros

object PrintMac {
  def apply(value: Any): Unit = macro PrintMacMacro.apply
  def detail(value: Any): Unit = macro PrintMacMacro.detail
}
