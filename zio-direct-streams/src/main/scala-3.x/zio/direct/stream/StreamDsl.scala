package zio.direct.stream

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.stream.ZStream
import zio.direct.core.NotDeferredException

object defer extends deferCall[ZStream, ZStream[?, ?, ?], Nothing, Nothing, StreamMonad.StreamMonadModel, Nothing] {
  def success = StreamMonad.Success
  def fallible = StreamMonad.Fallible
  def sequence = StreamMonad.Sequence
  def sequencePar = StreamMonad.SequencePar
  def state = ???
  def log = ???
}

extension [R, E, A](value: ZStream[R, E, A]) {
  @directRunCall
  def each: A = NotDeferredException.fromNamed("each")
}
