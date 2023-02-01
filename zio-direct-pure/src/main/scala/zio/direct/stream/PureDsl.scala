package zio.direct.pure

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.prelude.fx.ZPure
import zio.direct.core.NotDeferredException

class deferFactory[W, S] extends deferCall[[R, E, A] =>> ZPure[W, S, S, R, E, A], ZPure[?, ?, ?, ?, ?, ?]]

type ZPureProxy[R, E, A] = ZPure[_, _, _, R, E, A]

extension [R, E, A](value: ZPureProxy[R, E, A]) {
  @directRunCall
  def runPure: A = NotDeferredException.fromNamed("runPure")
}
