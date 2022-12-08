package zio.direct.core

import scala.reflect.macros.whitebox.Context
import zio.direct.core.Transformer

class Macro(val c: Context) extends Transformer {
  import c.universe.Tree

  def defer[T](value: Tree): Tree = {
    apply(value)
  }
}
