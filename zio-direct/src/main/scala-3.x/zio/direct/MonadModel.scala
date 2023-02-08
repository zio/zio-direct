package zio.direct

import zio.Task
import scala.quoted._
import zio.direct.core.Transformer
import zio.ZIO
import zio.direct.core.metaprog.Instructions
import zio.direct.core.metaprog.InfoBehavior
import zio.direct.core.metaprog.Collect
import zio.direct.core.metaprog.Unliftables
import zio.direct.core.metaprog.Verify
import zio.direct.core.NotDeferredException
import zio.direct.core.util.TraceType
import zio.direct.core.metaprog.TypeUnion
import zio.direct.core.metaprog.RefineInstructions

trait MonadModel[F[_, _, _]] {
  type Variances <: MonadShape.Variances
  type Letters <: MonadShape.Letters
}

trait MonadSuccess[F[_, _, _]] {
  def unit[A](a: => A): F[Any, Nothing, A]
  def map[R, E, A, B](first: F[R, E, A])(map: A => B): F[R, E, B]
  def flatMap[R, E, A, B](first: F[R, E, A])(andThen: A => F[R, E, B]): F[R, E, B]
  def flatten[R, E, A, R1 <: R, E1 >: E](first: F[R, E, F[R1, E1, A]]): F[R1, E1, A] // = flatMap(first)(x => x)
}

trait MonadFallible[F[_, _, _]] {
  def fail[E](e: => E): F[Any, E, Nothing]
  def attempt[A](a: => A): F[Any, Throwable, A]
  def catchSome[R, E, A](first: F[R, E, A])(andThen: PartialFunction[E, F[R, E, A]]): F[R, E, A]
  def ensuring[R, E, A](f: F[R, E, A])(finalizer: F[R, Nothing, Any]): F[R, E, A]
  def mapError[R, E, A, E2](first: F[R, E, A])(f: E => E2): F[R, E2, A]
  def orDie[R, E <: Throwable, A](first: F[R, E, A]): F[R, Nothing, A]
}

trait MonadState[F[_, _, _]] {
  def set[S](s: S): F[Any, Nothing, Unit]
  def get[S]: F[Any, Nothing, S]
}

trait MonadLog[F[_, _, _]] {
  def log[W](w: W): F[Any, Nothing, Unit]
}

trait MonadSequence[F[_, _, _]] {
  def foreach[R, E, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(f: A => F[R, E, B])(
      implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]
  ): F[R, E, Collection[B]]
  def collectAll[R, E, A, Collection[+Element] <: Iterable[Element]](in: Collection[F[R, E, A]])(
      implicit bf: scala.collection.BuildFrom[Collection[F[R, E, A]], A, Collection[A]]
  ): F[R, E, Collection[A]] = foreach(in)(x => x)
}

trait MonadSequenceParallel[F[_, _, _]] {
  def foreachPar[R, E, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(f: A => F[R, E, B])(
      implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]
  ): F[R, E, Collection[B]]
  def collectAllPar[R, E, A, Collection[+Element] <: Iterable[Element]](in: Collection[F[R, E, A]])(
      implicit bf: scala.collection.BuildFrom[Collection[F[R, E, A]], A, Collection[A]]
  ): F[R, E, Collection[A]] = foreachPar(in)(x => x)
}
