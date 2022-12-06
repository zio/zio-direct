package zio.direct.core

import scala.reflect.macros.whitebox.Context
import zio.direct.core.Transformer

private[direct] class Macro(val c: Context) extends Transformer {
  import c.universe.{Transformer => STransformer, _}

  def defer[T](value: Tree): Tree = {
    apply(value)
  }
}
