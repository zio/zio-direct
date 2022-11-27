package zio.direct.examples

import zio.ZIO

object RunNow {
  def apply[A](op: ZIO[Any, Throwable, A]) =
    zio.Unsafe.unsafe { implicit unsafe =>
      zio.Runtime.default.unsafe.run(op).getOrThrow()
    }
}
