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

object defer extends deferCall[[R, E, A] =>> Future[A], Future[?], Nothing, Nothing, FutureMonadModel] {
  transparent inline def success = futureSuccess(summonInline[ExecutionContext])
  transparent inline def fallible = Some(futureFallible(summonInline[ExecutionContext]))
  transparent inline def sequence = futureSequence(summonInline[ExecutionContext])
  transparent inline def sequencePar = futureSequencePar(summonInline[ExecutionContext])
  transparent inline def state = None
  transparent inline def log = None
}

extension [A](value: Future[A]) {
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
