package zio.direct.future

import scala.compiletime.summonInline
import zio.direct._
import MonadShape.Variance._
import MonadShape.Letter._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import zio.direct.core.NotDeferredException
import scala.util.Failure
import scala.util.Success

//transparent inline def defer(implicit inline ctx: ExecutionContext) = new deferCallImpl(ctx)

/**
 * Turns a block of code into a `Future`. Inside the block, `.run` on a `Future` continues once that future
 * completes and gives its value. An implicit `ExecutionContext` must be in scope.
 *
 * {{{
 * import scala.concurrent.{ExecutionContext, Future}
 * import zio.direct.future.{defer, run}
 *
 * def sum(implicit ec: ExecutionContext): Future[Int] = defer {
 *   val a = Future(1).run
 *   val b = Future(2).run
 *   a + b
 * }
 * }}}
 */
object defer extends deferCall[[R, E, A] =>> Future[A], Future[?], Nothing, Nothing, FutureMonadModel] {

  /**
   * Monad hook for `Future`. See `deferCall`.
   */
  transparent inline def success = futureSuccess(summonInline[ExecutionContext])

  /**
   * Monad hook for `Future`. See `deferCall`.
   */
  transparent inline def fallible = Some(futureFallible(summonInline[ExecutionContext]))

  /**
   * Monad hook for `Future`. See `deferCall`.
   */
  transparent inline def sequence = futureSequence(summonInline[ExecutionContext])

  /**
   * Monad hook for `Future`. See `deferCall`.
   */
  transparent inline def sequencePar = futureSequencePar(summonInline[ExecutionContext])

  /**
   * Monad hook for `Future`. See `deferCall`.
   */
  transparent inline def state = None

  /**
   * Monad hook for `Future`. See `deferCall`.
   */
  transparent inline def log = None
}

extension [A](value: Future[A]) {

  /**
   * Gives the value of this `Future` inside a `defer { ... }` block. Outside one, it throws `NotDeferredException`.
   */
  @directRunCall
  def run: A = NotDeferredException.fromNamed("run")
}

type ThreeFuture[R, E, A] = Future[A]
type FutureMonadModel = MonadModel {
  type Variances = MonadShape.Variances1[Covariant]
  type Letters = MonadShape.Letters1[A]
  type IsFallible = true
  type IsStateful = false
  type IsLogging = false
}
