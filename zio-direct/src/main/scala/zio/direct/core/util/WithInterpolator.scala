package zio.direct.core.util

import zio.direct.core.metaprog.WithIR
import zio.direct.core.metaprog.WithF
import zio.direct.core.metaprog.WithPrintIR
import zio.direct.core.metaprog.WithZioType
import zio.direct.core.norm.WithComputeType

trait WithInterpolator extends WithInterpolatorBase {
  self: WithF with WithIR with WithZioType with WithPrintIR =>

  override def printAny(any: Any): String = PrintAny(any)
}
