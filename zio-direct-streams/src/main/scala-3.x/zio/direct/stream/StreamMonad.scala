package zio.direct.stream

import zio.direct._
import zio.stream.ZStream
import zio.ZIO

import MonadShape.Variance._
import MonadShape.Letter._
import zio.Chunk

object StreamMonad {

  type StreamMonadModel = MonadModel {
    type Variances = MonadShape.Variances3[Contravariant, Covariant, Covariant]
    type Letters = MonadShape.Letters3[R, E, A]
    type IsFallible = true
    type IsStateful = false
    type IsLogging = false
  }

  val Success: MonadSuccess[ZStream] = new MonadSuccess[ZStream] {
    def unit[A](a: => A): ZStream[Any, Nothing, A] = ZStream.succeed[A](a)
    def map[R, E, A, B](first: ZStream[R, E, A])(andThen: A => B): ZStream[R, E, B] = first.map[B](andThen)
    def flatMap[R, E, A, B](first: ZStream[R, E, A])(andThen: A => ZStream[R, E, B]): ZStream[R, E, B] = first.flatMap[R, E, B](andThen)
    def flatten[R, E, A, R1 <: R, E1 >: E](first: ZStream[R, E, ZStream[R1, E1, A]]): ZStream[R1, E1, A] = first.flatten
  }

  val Fallible: MonadFallible[ZStream] = new MonadFallible[ZStream] {
    def fail[E](e: => E): ZStream[Any, E, Nothing] = ZStream.fail(e)
    def attempt[A](a: => A): ZStream[Any, Throwable, A] = ZStream.fromZIO(ZIO.attempt[A](a))
    def catchSome[R, E, A](first: ZStream[R, E, A])(andThen: PartialFunction[E, ZStream[R, E, A]]): ZStream[R, E, A] = first.catchSome[R, E, A](andThen)
    // finalizer here is a ZIO. How should this be encapsulated? does it need a special type?
    def ensuring[R, E, A](f: ZStream[R, E, A])(finalizer: ZStream[R, Nothing, Any]): ZStream[R, E, A] = f.ensuring(finalizer.runHead)
    def mapError[R, E, A, E2](first: ZStream[R, E, A])(f: E => E2): ZStream[R, E2, A] = first.mapError(f)
    def orDie[R, E <: Throwable, A](first: ZStream[R, E, A]): ZStream[R, Nothing, A] = first.orDie
  }

  val Sequence: MonadSequence[ZStream] = new MonadSequence[ZStream] {
    // basically the equivalent of `gens.foldRight[Gen[R, List[A]]](Gen.const(List.empty))(_.zipWith(_)(_ :: _))`
    private def crossN[R, E, A, B, C](streams: Chunk[ZStream[R, E, A]]): ZStream[R, E, Chunk[A]] =
      streams.foldLeft[ZStream[R, E, Chunk[A]]](ZStream.succeed(Chunk.empty)) { (left, right) => left.cross(right).map { case (l, r) => l :+ r } }

    def foreach[R, E, A, B, Collection[+Element] <: Iterable[Element]](
        in: Collection[A]
    )(f: A => ZStream[R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ZStream[R, E, Collection[B]] =
      // If exceptions thrown here, want to catch them when we wrap this
      lazy val crossedChunks = crossN(Chunk.fromIterable(in.map(f)))
      lazy val output = crossedChunks.map(chunk => bf.fromSpecific(in)(chunk))
      ZStream(output).flatten
  }

  val SequencePar: MonadSequenceParallel[ZStream] = new MonadSequenceParallel[ZStream] {
    def foreachPar[R, E, A, B, Collection[+Element] <: Iterable[Element]](
        in: Collection[A]
    )(f: A => ZStream[R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ZStream[R, E, Collection[B]] =
      // TODO Same problem again. Need a different type for the finalization
      Sequence.foreach(in)(f)
  }
}
