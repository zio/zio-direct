package zio.direct.core.util

import zio.ZIO
import zio.direct.MonadFallible

object ZioUtil {
  def wrapWithThrowable[R, E, A](zio: ZIO[R, E, A]) =
    zio.mapError {
      case t: Throwable => t
      case other        => new RuntimeException(s"ZIO failed due to: $other")
    }
}

object F3Util {
  def wrapWithThrowable[F[_, _, _], R, E, A](input: F[R, E, A])(implicit monad: MonadFallible[F]): F[R, Throwable, A] =
    monad.mapError(input) {
      case t: Throwable => t
      case other        => new RuntimeException(s"ZIO failed due to: $other")
    }
}
