package zio.direct

import zio.ZIO

/**
 * Turns a block of code into a `ZIO` effect. Inside the block, `.run` on a `ZIO` runs it and gives its value,
 * so the block reads like plain sequential code.
 *
 * {{{
 * import zio._
 * import zio.direct._
 *
 * def addTwo(a: ZIO[Any, Nothing, Int]): ZIO[Any, Nothing, Int] = defer {
 *   a.run + 2
 * }
 * }}}
 *
 * The other `defer` objects (streams, futures and pure) have the same shape, with their own unwrap method.
 * See `deferCall` for the methods.
 */
object defer extends deferCall[ZIO, ZIO[?, ?, ?], Nothing, Nothing, ZioMonad.ZioMonadModel] {

  /**
   * Monad hook for `ZIO`. See `deferCall`.
   */
  transparent inline def success = ZioMonad.Success

  /**
   * Monad hook for `ZIO`. See `deferCall`.
   */
  transparent inline def fallible = Some(ZioMonad.Fallible)

  /**
   * Monad hook for `ZIO`. See `deferCall`.
   */
  transparent inline def sequence = ZioMonad.Sequence

  /**
   * Monad hook for `ZIO`. See `deferCall`.
   */
  transparent inline def sequencePar = ZioMonad.SequenceParallel

  /**
   * Monad hook for `ZIO`. See `deferCall`.
   */
  transparent inline def state = None

  /**
   * Monad hook for `ZIO`. See `deferCall`.
   */
  transparent inline def log = None
}
