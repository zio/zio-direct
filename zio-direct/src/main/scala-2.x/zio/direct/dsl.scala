package zio

import zio.direct.core.NotDeferredException
import language.experimental.macros
import zio.ZIO

package object direct {
  def unsafe[T](value: T): T = NotDeferredException.fromNamed("unsafe")

  object defer {
    def apply[T](value: T): ZIO[_, _, _] = macro core.Macro.defer[T]
    def use[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.deferWithUse[T]

    object tpe {
      def apply[T](value: T): ZIO[_, _, _] = macro core.Macro.tpe[T]
      def use[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.tpeWithUse[T]
    }

    object info {
      def apply[T](value: T): ZIO[_, _, _] = macro core.Macro.info[T]
      def use[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.infoWithUse[T]
    }

    object verbose {
      def apply[T](value: T): ZIO[_, _, _] = macro core.Macro.verbose[T]
      def use[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.verboseWithUse[T]
    }

    object verboseTree {
      def apply[T](value: T): ZIO[_, _, _] = macro core.Macro.verboseTree[T]
      def use[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.verboseTreeWithUse[T]
    }
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
