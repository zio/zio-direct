package zio.direct.pure

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.direct.directGetCall
import zio.direct.directSetCall
import zio.direct.directLogCall
import zio.prelude.fx.ZPure
import zio.direct.core.NotDeferredException

// Using this plainly can possibly case a cyclical macro error? Not sure why
// class deferWithParams[W, S] extends deferCall[[R, E, A] =>> ZPure[W, S, S, R, E, A], ZPure[?, ?, ?, ?, ?, ?], S, W]
// object deferWithParams {
//   def apply[W, S] = new deferWithParams[W, S]
// }

class deferWithParams[W, S] extends deferCall[[R, E, A] =>> ZPure[W, S, S, R, E, A], ZPure[?, ?, ?, ?, ?, ?], S, W, PureMonad.PureMonadModel] {
  transparent inline def success = PureMonad.Success[W, S]
  transparent inline def fallible = Some(PureMonad.Fallible[W, S]) // Performance issues with try-rewrites
  transparent inline def sequence = PureMonad.Sequence[W, S]
  transparent inline def sequencePar = PureMonad.SequencePar[W, S]
  transparent inline def state = Some(PureMonad.State[W, S])
  transparent inline def log = Some(PureMonad.Log[W, S])
}

class deferWith[W, S] {
  val defer = new deferWithParams[W, S]

  // Note that initially it was attempted to implement setState and getState using `transparent inline def`
  // (just `inline def` does not work) the approach was much simpler as it looked like:
  //   transparent inline def setState(inline s: State) = summon[MonadState[F]].set(s)
  // however that implementation significantly slowed down
  // auto-completion speed or Metals dialog so instead the annotation method was introduced.
  // Also this method should have a similar annotation in Scala-2.

  /** Helper method to set the state */
  @directSetCall
  def setState(s: S): Unit = ZPure.set(s).eval

  /** Helper method to get the state */
  @directGetCall
  def getState(): S = ZPure.get[S].eval

  /** Helper method to do logging */
  @directLogCall
  def log(w: W): Unit = ZPure.log(w).eval

  object Wrap {
    def succeed[T](value: T) = ZPure.succeed[S, T](value)
    def attempt[T](value: T) = ZPure.attempt[S, T](value)
  }
}

type ZPureProxy[R, E, A] = ZPure[_, _, _, R, E, A]

extension [R, E, A](value: ZPureProxy[R, E, A]) {
  @directRunCall
  def eval: A = NotDeferredException.fromNamed("eval")
}
