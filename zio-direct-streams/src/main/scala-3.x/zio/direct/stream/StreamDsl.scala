package zio.direct.stream

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.stream.ZStream
import zio.direct.core.NotDeferredException

/**
 * Turns a block of code into a `ZStream`. Inside the block, `.each` on a stream takes its next element. The
 * rest of the block runs once for each element, so two `each` calls give every pair.
 *
 * {{{
 * import zio.direct.stream.{defer, each}
 * import zio.stream.ZStream
 *
 * val pairs = defer {
 *   val a = ZStream(1, 2).each
 *   val b = ZStream("x", "y").each
 *   (a, b)
 * }
 * }}}
 */
object defer extends deferCall[ZStream, ZStream[?, ?, ?], Nothing, Nothing, StreamMonad.StreamMonadModel] {

  /**
   * Monad hook for `ZStream`. See `deferCall`.
   */
  transparent inline def success = StreamMonad.Success

  /**
   * Monad hook for `ZStream`. See `deferCall`.
   */
  transparent inline def fallible = Some(StreamMonad.Fallible)

  /**
   * Monad hook for `ZStream`. See `deferCall`.
   */
  transparent inline def sequence = StreamMonad.Sequence

  /**
   * Monad hook for `ZStream`. See `deferCall`.
   */
  transparent inline def sequencePar = StreamMonad.SequencePar

  /**
   * Monad hook for `ZStream`. See `deferCall`.
   */
  transparent inline def state = None

  /**
   * Monad hook for `ZStream`. See `deferCall`.
   */
  transparent inline def log = None
}

extension [R, E, A](value: ZStream[R, E, A]) {

  /**
   * Takes the next element of this stream inside a `defer { ... }` block. The rest of the block runs once for
   * each element. Outside a `defer` block it throws `NotDeferredException`.
   */
  @directRunCall
  def each: A = NotDeferredException.fromNamed("each")
}
