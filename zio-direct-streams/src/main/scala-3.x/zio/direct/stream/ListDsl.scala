package zio.direct.stream

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.stream.ZStream
import zio.direct.core.NotDeferredException

object select extends deferCall[[R, E, A] =>> List[A], List[?]]

extension [R, E, A](value: List[A]) {
  @directRunCall
  def from: A = NotDeferredException.fromNamed("from")
}
