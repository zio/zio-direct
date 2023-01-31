package zio.direct.stream

import zio.direct._
import zio.stream.ZStream
import zio.ZIO

import MonadShape.Variance._
import MonadShape.Letter._
implicit val streamMonadModel: MonadModel[ZStream] {
  type Variances = MonadShape.Variances3[Contravariant, Covariant, Covariant]
  type Letters = MonadShape.Letters3[R, E, A]
} = new MonadModel[ZStream] {
  type Variances = MonadShape.Variances3[Contravariant, Covariant, Covariant]
  type Letters = MonadShape.Letters3[R, E, A]
}

implicit val zstreamMonadSuccess: MonadSuccess[ZStream] = new MonadSuccess[ZStream] {
  def unit[A](a: => A): ZStream[Any, Nothing, A] = ZStream.succeed[A](a)
  def map[R, E, A, B](first: ZStream[R, E, A])(andThen: A => B): ZStream[R, E, B] = first.map[B](andThen)
  def flatMap[R, E, A, B](first: ZStream[R, E, A])(andThen: A => ZStream[R, E, B]): ZStream[R, E, B] = first.flatMap[R, E, B](andThen)
  def flatten[R, E, A, R1 <: R, E1 >: E](first: ZStream[R, E, ZStream[R1, E1, A]]): ZStream[R1, E1, A] = first.flatten
}

implicit val zstreamMonadFallible: MonadFallible[ZStream] = new MonadFallible[ZStream] {
  def fail[E](e: => E): ZStream[Any, E, Nothing] = ZStream.fail(e)
  def attempt[A](a: => A): ZStream[Any, Throwable, A] = ZStream.fromZIO(ZIO.attempt[A](a))
  def catchSome[R, E, A](first: ZStream[R, E, A])(andThen: PartialFunction[E, ZStream[R, E, A]]): ZStream[R, E, A] = first.catchSome[R, E, A](andThen)
  // finalizer here is a ZIO. How should this be encapsulated? does it need a special type?
  def ensuring[R, E, A](f: ZStream[R, E, A])(finalizer: ZStream[R, Nothing, Any]): ZStream[R, E, A] = f.ensuring(finalizer.runHead)
  def mapError[R, E, A, E2](first: ZStream[R, E, A])(f: E => E2): ZStream[R, E2, A] = first.mapError(f)
  def orDie[R, E <: Throwable, A](first: ZStream[R, E, A]): ZStream[R, Nothing, A] = first.orDie
}

implicit val zstreamMonadSequence: MonadSequence[ZStream] = new MonadSequence[ZStream] {
  def foreach[R, E, A, B, Collection[+Element] <: Iterable[Element]](
      in: Collection[A]
  )(f: A => ZStream[R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ZStream[R, E, Collection[B]] =
    // TODO Same problem again. Need a different type for the finalization
    ZStream.fromZIO(ZIO.foreach(in)((a: A) => f(a).runHead.map(_.get)))
}

implicit val zstreamMonadSequencePar: MonadSequenceParallel[ZStream] = new MonadSequenceParallel[ZStream] {
  def foreachPar[R, E, A, B, Collection[+Element] <: Iterable[Element]](
      in: Collection[A]
  )(f: A => ZStream[R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ZStream[R, E, Collection[B]] =
    // TODO Same problem again. Need a different type for the finalization
    ZStream.fromZIO(ZIO.foreachPar(in)((a: A) => f(a).runHead.map(_.get)))
}
