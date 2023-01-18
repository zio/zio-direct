package zio.direct.core.util

import zio.direct.core.metaprog.WithIR
import zio.direct.core.metaprog.WithPrintIR
import zio.direct.core.metaprog.WithZioType
import zio.direct.core.norm.WithComputeType

trait WithInterpolator extends WithInterpolatorBase {
  self: WithIR with WithZioType with WithPrintIR =>

  override def printAny(any: Any): String = PrintAny(any)
}
