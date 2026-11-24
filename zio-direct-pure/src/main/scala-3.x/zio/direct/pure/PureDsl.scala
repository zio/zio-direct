package zio.direct.pure

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.direct.directGetCall
import zio.direct.directSetCall
import zio.direct.directLogCall
import zio.prelude.fx.ZPure
import zio.direct.core.NotDeferredException
import zio.Tag

// Using this plainly can possibly case a cyclical macro error? Not sure why
// class deferWithParams[W, S] extends deferCall[[R, E, A] =>> ZPure[W, S, S, R, E, A], ZPure[?, ?, ?, ?, ?, ?], S, W]
// object deferWithParams {
//   def apply[W, S] = new deferWithParams[W, S]
// }

/**
 * The `defer` block builder for a `deferWith[W, S]` context. Blocks build a `ZPure[W, S, S, R, E, A]`, where
 * `W` is the log type and `S` is the state type. Get one through `deferWith`, not by constructing it.
 */
class deferWithParams[W, S] extends deferCall[[R, E, A] =>> ZPure[W, S, S, R, E, A], ZPure[?, ?, ?, ?, ?, ?], S, W, PureMonad.PureMonadModel] {

  /**
   * Monad hook for `ZPure`. See `deferCall`.
   */
  transparent inline def success = PureMonad.Success[W, S]

  /**
   * Monad hook for `ZPure`. See `deferCall`.
   */
  transparent inline def fallible = Some(PureMonad.Fallible[W, S])

  /**
   * Monad hook for `ZPure`. See `deferCall`.
   */
  transparent inline def sequence = PureMonad.Sequence[W, S]

  /**
   * Monad hook for `ZPure`. See `deferCall`.
   */
  transparent inline def sequencePar = PureMonad.SequencePar[W, S]

  /**
   * Monad hook for `ZPure`. See `deferCall`.
   */
  transparent inline def state = Some(PureMonad.State[W, S])

  /**
   * Monad hook for `ZPure`. See `deferCall`.
   */
  transparent inline def log = Some(PureMonad.Log[W, S])
}

/**
 * A context for `ZPure` computations that share a state of type `S` and a log of type `W`.
 *
 * `defer` builds the blocks of this context. Inside them, `getState()`, `setState(...)`, `log(...)` and the
 * `Wrap` helpers are available. Import the members of the context with `import dc._`.
 *
 * {{{
 * import zio.direct.pure.deferWith
 *
 * val dc = deferWith[String, Int]
 * import dc._
 *
 * val program = defer {
 *   val before = getState()
 *   setState(before + 1)
 *   log("incremented")
 *   before
 * }
 * }}}
 */
class deferWith[W, S] {

  /**
   * The `defer` block builder for this context. See `deferWith`.
   */
  val defer = new deferWithParams[W, S]

  // Note that initially it was attempted to implement setState and getState using `transparent inline def`
  // (just `inline def` does not work) the approach was much simpler as it looked like:
  //   transparent inline def setState(inline s: State) = summon[MonadState[F]].set(s)
  // however that implementation significantly slowed down
  // auto-completion speed or Metals dialog so instead the annotation method was introduced.
  // Also this method should have a similar annotation in Scala-2.

  /**
   * Sets the state to `s`. Use it inside a `defer { ... }` block. Outside one, it throws `NotDeferredException`.
   */
  @directSetCall
  def setState(s: S): Unit = ZPure.set(s).eval

  /**
   * Gives the current state. Use it inside a `defer { ... }` block.
   */
  @directGetCall
  def getState(): S = ZPure.get[S].eval

  /**
   * Adds `w` to the log. Use it inside a `defer { ... }` block.
   */
  @directLogCall
  def log(w: W): Unit = ZPure.log(w).eval

  /**
   * Helpers that turn a plain value into a `ZPure` step. Inside a `defer { ... }` block, call `.eval` on the
   * step to run it, as in `Wrap.succeed(3).eval`. The `eval` extension is in `zio.direct.pure`.
   */
  object Wrap {

    /**
     * A step that succeeds with `value`.
     */
    def succeed[T](value: T) = ZPure.succeed[S, T](value)

    /**
     * A step that holds `value`. The value is computed before the step is built, so an exception thrown while
     * computing it is not turned into a failure here.
     */
    def attempt[T](value: T) = ZPure.attempt[S, T](value)

    /**
     * A step that reads a service of type `T` from the environment.
     */
    def service[T: Tag] = ZPure.service[S, T]
  }
}

/**
 * The type that the `eval` extension applies to: any `ZPure`, whatever its state, log and error types.
 */
type ZPureProxy[R, E, A] = ZPure[_, _, _, R, E, A]

extension [R, E, A](value: ZPureProxy[R, E, A]) {

  /**
   * Runs this `ZPure` step inside a `defer { ... }` block and gives its value. Outside one, it throws
   * `NotDeferredException`.
   */
  @directRunCall
  def eval: A = NotDeferredException.fromNamed("eval")
}
