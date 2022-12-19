package zio.direct.core.util.debug

import scala.language.experimental.macros

object PrintMac {
  def apply(valueRaw: Any): Unit = macro PrintMacMacro.apply
  def detail(valueRaw: Any): Unit = macro PrintMacMacro.detail
}
