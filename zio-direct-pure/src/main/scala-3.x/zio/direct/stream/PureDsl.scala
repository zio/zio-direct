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

class deferWith[W, S] {
  object defer extends deferCall[[R, E, A] =>> ZPure[W, S, S, R, E, A], ZPure[?, ?, ?, ?, ?, ?], S, W, PureMonadModel](
        zpureMonadSuccess[W, S],
        Some(zpureMonadFallible[W, S]), // MUCH better perf when this is removed
        zpureMonadSequence[W, S],
        zpureMonadSequencePar[W, S],
        Some(zpureMonadState[W, S]),
        Some(zpureMonadLog[W, S])
      )
  object State {
    // Note that initially it was attempted to implement these things using `transparent inline def`
    // (just `inline def` does not work) however that implementation significantly slowed down
    // auto-completion speed or Metals dialog so instead the annotation method was introduced.
    // Also this method should have a similar annotation in Scala-2.

    /** Helper method to set the state */
    @directSetCall
    def set(s: S): Unit = ZPure.set(s).eval

    /** Helper method to get the state */
    @directGetCall
    def get(): S = ZPure.get[S].eval
  }

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
