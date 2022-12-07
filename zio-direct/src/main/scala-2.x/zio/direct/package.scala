package zio

import zio.direct.core.NotDeferredException
import language.experimental.macros
import zio.ZIO

package object direct {
  def unsafe[T](value: T): T = NotDeferredException.fromNamed("unsafe")

  object defer {
    def apply[T](value: T): ZIO[_, _, _] = macro core.Macro.defer[T]
  }

  implicit class ZioRunOps[R, E, A](value: ZIO[R, E, A]) {
    def run: A = NotDeferredException.fromNamed("run")
  }

  def run[R, E, A](value: ZIO[R, E, A]): A = NotDeferredException.fromNamed("run")

  object Internal {
    def deferred[R, E, A](effect: ZIO[R, E, A]) = effect
    def ignore[T](code: T): T =
      throw new NotDeferredException(s"The construct `ignore` be used inside of a `defer { ... }` block and should only be used for testing purposes!")
  }
}
