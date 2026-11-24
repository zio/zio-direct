package zio

import zio.direct.core.NotDeferredException
import language.experimental.macros
import zio.ZIO

package object direct {

  /**
   * Runs a block as plain code inside a `defer { ... }` block, not as effects. An exception thrown inside
   * becomes a failure of the effect. Outside a `defer` block it throws `NotDeferredException`.
   */
  def unsafe[T](value: T): T = NotDeferredException.fromNamed("unsafe")

  /**
   * The Scala 2 entry point. `defer(value)` is a macro that turns the block into a `ZIO`, the same as `defer`
   * on Scala 3. The overloads that take a `Use` accept the settings described on `Use`.
   */
  object defer {

    /**
     * Turns `value` into a `ZIO`. Each `run` call inside becomes a step of the result.
     */
    def apply[T](value: T): ZIO[_, _, _] = macro core.Macro.defer[T]

    /**
     * Same as `apply`, and prints the type the block computes at compile time.
     */
    def tpe[T](value: T): ZIO[_, _, _] = macro core.Macro.tpe[T]

    /**
     * Same as `apply`, and prints compile-time information about the transformation.
     */
    def info[T](value: T): ZIO[_, _, _] = macro core.Macro.info[T]

    /**
     * Same as `info`, with more compile-time detail.
     */
    def verbose[T](value: T): ZIO[_, _, _] = macro core.Macro.verbose[T]

    /**
     * Same as `verbose`, and also prints the generated code as a tree.
     */
    def verboseTree[T](value: T): ZIO[_, _, _] = macro core.Macro.verboseTree[T]

    /**
     * Same as `apply(value)`, with the settings in `use`. See `Use`.
     */
    def apply[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.deferWithUse[T]

    /**
     * Same as `tpe(value)`, with the settings in `use`. See `Use`.
     */
    def tpe[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.tpeWithUse[T]

    /**
     * Same as `info(value)`, with the settings in `use`. See `Use`.
     */
    def info[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.infoWithUse[T]

    /**
     * Same as `verbose(value)`, with the settings in `use`. See `Use`.
     */
    def verbose[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.verboseWithUse[T]

    /**
     * Same as `verboseTree(value)`, with the settings in `use`. See `Use`.
     */
    def verboseTree[T](use: Use)(value: T): ZIO[_, _, _] = macro core.Macro.verboseTreeWithUse[T]
  }

  /**
   * Adds `.run` to a `ZIO` inside a `defer { ... }` block. Outside one, `run` throws `NotDeferredException`.
   */
  implicit class ZioRunOps[R, E, A](value: ZIO[R, E, A]) {

    /**
     * Gives the value of this `ZIO` inside a `defer { ... }` block. Outside one, it throws `NotDeferredException`.
     */
    def run: A = NotDeferredException.fromNamed("run")
  }

  /**
   * Function form of `.run`: `run(effect)` inside a `defer { ... }` block gives the value of `effect`. Outside
   * one, it throws `NotDeferredException`.
   */
  def run[R, E, A](value: ZIO[R, E, A]): A = NotDeferredException.fromNamed("run")

  object Internal {
    def deferred[R, E, A](effect: ZIO[R, E, A]) = effect
    def ignore[T](code: T): T =
      throw new NotDeferredException(s"The construct `ignore` be used inside of a `defer { ... }` block and should only be used for testing purposes!")
  }
}
