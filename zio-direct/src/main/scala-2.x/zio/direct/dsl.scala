package zio

import zio.direct.core.NotDeferredException
import language.experimental.macros
import zio.ZIO

package object direct {
  def unsafe[T](value: T): T = NotDeferredException.fromNamed("unsafe")

  object defer {
    def apply[T](value: T): ZIO[_, _, _] = macro core.Macro.defer[T]
    def tpe[T](value: T): ZIO[_, _, _] = macro core.Macro.tpe[T]
    def info[T](value: T): ZIO[_, _, _] = macro core.Macro.info[T]
    def verbose[T](value: T): ZIO[_, _, _] = macro core.Macro.verbose[T]
    def verboseTree[T](value: T): ZIO[_, _, _] = macro core.Macro.verboseTree[T]

    def apply[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.deferWithUse[T]
    def tpe[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.tpeWithUse[T]
    def info[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.infoWithUse[T]
    def verbose[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.verboseWithUse[T]
    def verboseTree[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.verboseTreeWithUse[T]
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
