package zio.direct.list

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.direct.core.NotDeferredException

object select extends deferCall[[R, E, A] =>> List[A], List[?], Nothing, Nothing]

extension [R, E, A](value: List[A]) {
  @directRunCall
  def from: A = NotDeferredException.fromNamed("from")
}
