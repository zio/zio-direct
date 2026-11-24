package zio.direct

import zio.direct.core.NotDeferredException

/**
 * Settings for `defer`, passed as `defer(Use.withLenientCheck) { ... }` and similar. Each `with` method
 * returns a new `Use`, so settings chain: `Use.withLenientCheck.withParallelEval`. When two settings change
 * the same thing, the one written last wins.
 *
 * {{{
 * import zio._
 * import zio.direct._
 *
 * val program: ZIO[Any, Nothing, Int] = defer(Use.withLenientCheck.withParallelEval) {
 *   var count = 0
 *   count = count + 1
 *   val a = ZIO.succeed(1).run
 *   count + a
 * }
 * }}}
 */
trait Use {

  /**
   * Allows local `var`s, and assigning to them, inside the block. A `var` cannot be set from a `run` call. Read
   * the value into a `val` first.
   */
  def withLenientCheck: Use = NotDeferredException.fromNamed("Use.withLenientCheck")

  /**
   * Turns off the checks that reject unsupported code. Use it only when you have checked that the block is safe,
   * because the checks are what report unsupported code at compile time.
   */
  def withNoCheck: Use = NotDeferredException.fromNamed("Use.withNoCheck")

  /**
   * Runs the `run` calls that make up one expression, such as the tuple `(a.run, b.run)`, in parallel. In a
   * `for` loop over a collection, the body runs in parallel for each element. Without this setting they run one
   * after another. Only the ZIO `defer` runs them in parallel. The streams, futures, pure and list DSLs still run
   * them one after another.
   */
  def withParallelEval: Use = NotDeferredException.fromNamed("Use.withParallelEval")

  /**
   * Gives the error type of the result as the closest common supertype of the errors, instead of a union.
   * For example, `IOException` and `String` give `Serializable` instead of `IOException | String`.
   */
  def withAbstractError: Use = NotDeferredException.fromNamed("Use.withAbstractError")
}

/**
 * The default settings. Start a chain from here, as in `Use.withNoCheck`.
 */
object Use extends Use
