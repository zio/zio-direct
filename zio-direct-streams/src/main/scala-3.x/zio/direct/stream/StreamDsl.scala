package zio.direct.stream

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.stream.ZStream
import zio.direct.core.NotDeferredException

object defer extends deferCall[ZStream, ZStream[?, ?, ?], Nothing, Nothing, StreamMonad.StreamMonadModel] {
  transparent inline def success = StreamMonad.zstreamMonadSuccess
  transparent inline def fallible = Some(StreamMonad.zstreamMonadFallible)
  transparent inline def sequence = StreamMonad.zstreamMonadSequence
  transparent inline def sequencePar = StreamMonad.zstreamMonadSequencePar
  transparent inline def state = None
  transparent inline def log = None
}

extension [R, E, A](value: ZStream[R, E, A]) {
  @directRunCall
  def each: A = NotDeferredException.fromNamed("each")
}
