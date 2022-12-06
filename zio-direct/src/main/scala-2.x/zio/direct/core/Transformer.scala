package zio.direct.core

import zio.direct.core.metaprog.WithIR
import zio.direct.core.metaprog.WithPrintIR
import zio.direct.core.norm.WithDecomposeTree
import zio.direct.core.util.WithInterpolator
import zio.direct.core.metaprog.WithZioType
import zio.direct.core.util.WithFormat
import zio.direct.core.util.WithUnsupported
import zio.direct.core.metaprog.Instructions
import zio.direct.core.util.Announce

abstract class Transformer
    extends WithIR
    // with WithComputeType
    with WithPrintIR
    // with WithReconstructTree
    with WithDecomposeTree
    with WithInterpolator
    with WithZioType
    with WithFormat
    with WithUnsupported {

  import c.universe._

  protected def posFileStr(pos: Position): String = {
    val path = pos.source.path
    s"$path:${pos.line}:${pos.column}"
  }

  def apply[T](value: Tree): Tree = {
    val transformedRaw = Decompose(Instructions.default)(value)

    def fileShow = Announce.FileShow.FullPath(posFileStr(value.pos))

    println("=========== here =======")
    Announce.section("Deconstructed Instructions", PrintIR(transformedRaw), fileShow)

    q"???"
  }
}
