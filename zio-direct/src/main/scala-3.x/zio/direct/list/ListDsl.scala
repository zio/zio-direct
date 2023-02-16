package zio.direct.list

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.direct.core.NotDeferredException

object select extends deferCall[[R, E, A] =>> List[A], List[?], Nothing, Nothing, ListMonadModel] {
  transparent inline def success = listMonadSuccess
  transparent inline def fallible = None
  transparent inline def sequence = listMonadSequence
  transparent inline def sequencePar = listMonadSequencePar
  transparent inline def state = None
  transparent inline def log = None
}

extension [A](value: List[A]) {
  @directRunCall
  def from: A = NotDeferredException.fromNamed("from")
}
