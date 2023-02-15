package zio.direct.list

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.direct.core.NotDeferredException

object select extends deferCall[[R, E, A] =>> List[A], List[?], Nothing, Nothing, ListMonadModel, Nothing] {
  def success = listMonadSuccess
  def fallible = ???
  def sequence = listMonadSequence
  def sequencePar = listMonadSequencePar
  def state = ???
  def log = ???
}

extension [A](value: List[A]) {
  @directRunCall
  def from: A = NotDeferredException.fromNamed("from")
}
