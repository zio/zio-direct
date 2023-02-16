package zio.direct.stream

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.stream.ZStream
import zio.direct.core.NotDeferredException

object defer extends deferCall[ZStream, ZStream[?, ?, ?], Nothing, Nothing, StreamMonad.StreamMonadModel] {
  transparent inline def success = StreamMonad.Success
  transparent inline def fallible = Some(StreamMonad.Fallible)
  transparent inline def sequence = StreamMonad.Sequence
  transparent inline def sequencePar = StreamMonad.SequencePar
  transparent inline def state = None
  transparent inline def log = None
}

extension [R, E, A](value: ZStream[R, E, A]) {
  @directRunCall
  def each: A = NotDeferredException.fromNamed("each")
}
