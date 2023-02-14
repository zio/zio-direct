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
}

def futureSuccess(implicit ctx: ExecutionContext): MonadSuccess[ThreeFuture] = new MonadSuccess[ThreeFuture] {
  def unit[A](a: => A): ThreeFuture[Any, Nothing, A] = Future(a)
  def map[R, E, A, B](first: ThreeFuture[R, E, A])(andThen: A => B): ThreeFuture[R, E, B] = first.map[B](andThen)
  def flatMap[R, E, A, B](first: ThreeFuture[R, E, A])(andThen: A => ThreeFuture[R, E, B]): ThreeFuture[R, E, B] = first.flatMap[B](andThen)
  def flatten[R, E, A, R1 <: R, E1 >: E](first: ThreeFuture[R, E, ThreeFuture[R1, E1, A]]): ThreeFuture[R1, E1, A] = first.flatten
}

def futureFallible(implicit ctx: ExecutionContext): MonadFallible[ThreeFuture] = new MonadFallible[ThreeFuture] {
  def fail[E](e: => E): ThreeFuture[Any, Nothing, Nothing] =
    e match {
      case t: Throwable => Future.failed(t)
      case _            => Future.failed(new RuntimeException(s"Tried to fail future with non-throwable: ${e}"))
    }
  def attempt[A](a: => A): Future[A] = Future(a)

  def catchSome[R, E, A](first: Future[A])(andThen: PartialFunction[E, Future[A]]): Future[A] =
    val pf: PartialFunction[Throwable, E] = {
      case e: Throwable => e.asInstanceOf[E]
    }
    first.recoverWith(pf.andThen(andThen))

  def ensuring[R, E, A](f: Future[A])(finalizer: Future[Any]): Future[A] =
    f.transformWith {
      case Success(value)     => finalizer.flatMap(_ => Future(value))
      case Failure(exception) => finalizer.flatMap(_ => Future.failed(exception))
    }

  def mapError[R, E, A, E2](first: Future[A])(f: E => E2): Future[A] =
    first.transform {
      case s: Success[A]      => s
      case Failure(exception) => Failure(f(exception.asInstanceOf[E]).asInstanceOf[Throwable])
    }

  def orDie[R, E <: Throwable, A](first: Future[A]): Future[A] = first
}

def futureSequence(implicit ctx: ExecutionContext): MonadSequence[ThreeFuture] = new MonadSequence[ThreeFuture] {
  inline def foreach[R, E, A, B, Collection[+Element] <: Iterable[Element]](
      in: Collection[A]
  )(f: A => ThreeFuture[R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ThreeFuture[R, E, Collection[B]] =
    Future.sequence(in.map(f(_))).map(list => bf.fromSpecific(in)(list))
}

def futureSequencePar(implicit ctx: ExecutionContext): MonadSequenceParallel[ThreeFuture] = new MonadSequenceParallel[ThreeFuture] {
  def foreachPar[R, E, A, B, Collection[+Element] <: Iterable[Element]](
      in: Collection[A]
  )(f: A => ThreeFuture[R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ThreeFuture[R, E, Collection[B]] =
    futureSequence.foreach(in)(f)
}
