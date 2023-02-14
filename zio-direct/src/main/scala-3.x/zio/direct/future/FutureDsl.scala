package zio.direct.future

import scala.compiletime.summonInline
import zio.direct._
import MonadShape.Variance._
import MonadShape.Letter._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import zio.direct.core.NotDeferredException

object defer extends deferCall[[R, E, A] =>> Future[A], Future[?], Nothing, Nothing, FutureMonadModel] {
  transparent inline def success = futureSuccess(summonInline[ExecutionContext])
  transparent inline def fallible = None
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
  type IsFallible = false
}

def futureSuccess(implicit ctx: ExecutionContext): MonadSuccess[ThreeFuture] = new MonadSuccess[ThreeFuture] {
  def unit[A](a: => A): ThreeFuture[Any, Nothing, A] = Future(a)
  def map[R, E, A, B](first: ThreeFuture[R, E, A])(andThen: A => B): ThreeFuture[R, E, B] = first.map[B](andThen)
  def flatMap[R, E, A, B](first: ThreeFuture[R, E, A])(andThen: A => ThreeFuture[R, E, B]): ThreeFuture[R, E, B] = first.flatMap[B](andThen)
  def flatten[R, E, A, R1 <: R, E1 >: E](first: ThreeFuture[R, E, ThreeFuture[R1, E1, A]]): ThreeFuture[R1, E1, A] = first.flatten
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
