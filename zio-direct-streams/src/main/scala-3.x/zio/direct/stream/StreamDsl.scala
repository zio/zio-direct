package zio.direct.stream

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.stream.ZStream
import zio.direct.core.NotDeferredException

object defer extends deferCall[ZStream, ZStream[?, ?, ?], Nothing, Nothing, StreamMonad.StreamMonadModel] {
  inline def success = StreamMonad.Success
  inline def fallible = StreamMonad.Fallible
  inline def sequence = StreamMonad.Sequence
  inline def sequencePar = StreamMonad.SequencePar
  inline def state = zio.direct.FailUnused.forMonadState()
  inline def log = zio.direct.FailUnused.forMonadLog()
}

extension [R, E, A](value: ZStream[R, E, A]) {
  @directRunCall
  def each: A = NotDeferredException.fromNamed("each")
}
