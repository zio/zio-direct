package zio.direct.core.util

import zio.ZIO

object ZioUtil {
  def wrapWithThrowable[R, E, A](zio: ZIO[R, E, A]) =
    zio.mapError {
      case t: Throwable => t
      case other        => new RuntimeException(s"ZIO failed due to: $other")
    }
}
