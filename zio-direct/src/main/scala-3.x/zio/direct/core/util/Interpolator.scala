package zio.direct.core.util

import zio.direct.core.metaprog.WithIR
import scala.quoted._
import zio.direct.core.util.Format
import zio.ZIO
import zio.direct.core.metaprog.WithPrintIR
import zio.direct.core.metaprog.WithZioType
import java.io.PrintStream
import zio.direct.core.metaprog.Instructions
import scala.util.matching.Regex
import scala.collection.mutable.ArrayBuffer

trait WithInterpolator extends WithInterpolatorBase {
  self: WithIR with WithZioType with WithPrintIR =>

  override def printAny(any: Any): String = PrintAny(any)
}
