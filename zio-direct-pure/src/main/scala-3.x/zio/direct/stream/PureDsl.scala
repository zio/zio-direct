package zio.direct.pure

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.prelude.fx.ZPure
import zio.direct.core.NotDeferredException

class deferWith[W, S] extends deferCall[[R, E, A] =>> ZPure[W, S, S, R, E, A], ZPure[?, ?, ?, ?, ?, ?]]

class alwaysDeferWith[W, S] {
  def defer = deferWith[W, S]
}

type ZPureProxy[R, E, A] = ZPure[_, _, _, R, E, A]

extension [R, E, A](value: ZPureProxy[R, E, A]) {
  @directRunCall
  def eval: A = NotDeferredException.fromNamed("eval")
}
