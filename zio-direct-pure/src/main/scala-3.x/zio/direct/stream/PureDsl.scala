package zio.direct.pure

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.direct.directGetCall
import zio.direct.directSetCall
import zio.direct.directLogCall
import zio.prelude.fx.ZPure
import zio.direct.core.NotDeferredException

class deferWithParams[W, S] extends deferCall[[R, E, A] =>> ZPure[W, S, S, R, E, A], ZPure[?, ?, ?, ?, ?, ?]]
object deferWithParams {
  def apply[W, S] = new deferWithParams[W, S]
}

class deferWith[W, S] {
  def defer = deferWithParams[W, S]
  object State {
    @directSetCall
    def set(s: S): Unit = ZPure.set(s).eval
    @directGetCall
    def get(): S = ZPure.get[S].eval
  }
  @directLogCall
  def log(w: W): Unit = ZPure.log(w).eval
}

type ZPureProxy[R, E, A] = ZPure[_, _, _, R, E, A]

extension [R, E, A](value: ZPureProxy[R, E, A]) {
  @directRunCall
  def eval: A = NotDeferredException.fromNamed("eval")
}
