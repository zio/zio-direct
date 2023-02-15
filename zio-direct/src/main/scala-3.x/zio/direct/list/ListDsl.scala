package zio.direct.list

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.direct.core.NotDeferredException

object select extends deferCall[[R, E, A] =>> List[A], List[?], Nothing, Nothing, ListMonadModel] {
  inline def success = listMonadSuccess
  inline def fallible = zio.direct.FailUnused.forMonadFallible()
  inline def sequence = listMonadSequence
  inline def sequencePar = listMonadSequencePar
  inline def state = zio.direct.FailUnused.forMonadState()
  inline def log = zio.direct.FailUnused.forMonadLog()
}

extension [A](value: List[A]) {
  @directRunCall
  def from: A = NotDeferredException.fromNamed("from")
}
