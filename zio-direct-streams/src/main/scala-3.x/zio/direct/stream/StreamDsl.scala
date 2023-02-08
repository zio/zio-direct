package zio.direct.stream

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.stream.ZStream
import zio.direct.core.NotDeferredException

object defer extends deferCall[ZStream, ZStream[?, ?, ?], Nothing, Nothing](zstreamMonadSuccess, Some(zstreamMonadFallible), zstreamMonadSequence, zstreamMonadSequencePar, None, None)

extension [R, E, A](value: ZStream[R, E, A]) {
  @directRunCall
  def each: A = NotDeferredException.fromNamed("each")
}
