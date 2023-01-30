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

trait MonadSuccess[F[_, _, _]] {
  def unit[A](a: => A): F[Any, Nothing, A]
  def map[R, E, A, B](first: F[R, E, A])(map: A => B): F[R, E, B]
  def flatMap[R, E, A, B](first: F[R, E, A])(andThen: A => F[R, E, B]): F[R, E, B]
  def flatten[R, E, A, R1 <: R, E1 >: E](first: F[R, E, F[R1, E1, A]]): F[R1, E1, A] // = flatMap(first)(x => x)
}

trait MonadFallible[F[_, _, _]] {

  def fail[E](e: => E): F[Any, E, Nothing]
  // TODO Maybe for this level of API we should not care about variance?
  // TODO errors should be throwable types enforce via an implicit constrait since E <: Throwable doesn't work with type-matching
  // Trying to unify the two e-parameters (i.e. generalize the two into one another because otherwise it's difficult for the type-matching in Resolver)

  def attempt[A](a: => A): F[Any, Throwable, A]
  def catchSome[R, E, A](first: F[R, E, A])(andThen: PartialFunction[E, F[R, E, A]]): F[R, E, A]
  def ensuring[R, E, A](f: F[R, E, A])(finalizer: F[R, Nothing, Any]): F[R, E, A]
  def mapError[R, E, A, E2](first: F[R, E, A])(f: E => E2): F[R, E2, A]
  def orDie[R, E <: Throwable, A](first: F[R, E, A]): F[R, Nothing, A]
}

trait MonadSequence[F[_, _, _]] {
  // Should BuildFrom go here? Should we even allow multiple ZIOs on the same line? (which would require this)
  def foreach[R, E, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(f: A => F[R, E, B])(
      implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]
  ): F[R, E, Collection[B]]
  def collectAll[R, E, A, Collection[+Element] <: Iterable[Element]](in: Collection[F[R, E, A]])(
      implicit bf: scala.collection.BuildFrom[Collection[F[R, E, A]], A, Collection[A]]
  ): F[R, E, Collection[A]] = foreach(in)(x => x)
}

trait MonadSequenceParallel[F[_, _, _]] {
  // Should BuildFrom go here? Should we even allow multiple ZIOs on the same line? (which would require this)
  def foreachPar[R, E, A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(f: A => F[R, E, B])(
      implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]
  ): F[R, E, Collection[B]]
  def collectAllPar[R, E, A, Collection[+Element] <: Iterable[Element]](in: Collection[F[R, E, A]])(
      implicit bf: scala.collection.BuildFrom[Collection[F[R, E, A]], A, Collection[A]]
  ): F[R, E, Collection[A]] = foreachPar(in)(x => x)
}

implicit val zioMonadSuccess: MonadSuccess[ZIO] = new MonadSuccess[ZIO] {
  def unit[A](a: => A): ZIO[Any, Nothing, A] = ZIO.succeed[A](a)
  def map[R, E, A, B](first: ZIO[R, E, A])(andThen: A => B): ZIO[R, E, B] = first.map[B](andThen)
  def flatMap[R, E, A, B](first: ZIO[R, E, A])(andThen: A => ZIO[R, E, B]): ZIO[R, E, B] = first.flatMap[R, E, B](andThen)
  def flatten[R, E, A, R1 <: R, E1 >: E](first: ZIO[R, E, ZIO[R1, E1, A]]): ZIO[R1, E1, A] = first.flatten
}

implicit val zioMonadFallible: MonadFallible[ZIO] = new MonadFallible[ZIO] {
  def fail[E](e: => E): ZIO[Any, E, Nothing] = ZIO.fail(e)
  def attempt[A](a: => A): ZIO[Any, Throwable, A] = ZIO.attempt[A](a)
  def catchSome[R, E, A](first: ZIO[R, E, A])(andThen: PartialFunction[E, ZIO[R, E, A]]): ZIO[R, E, A] = first.catchSome[R, E, A](andThen)
  def ensuring[R, E, A](f: ZIO[R, E, A])(finalizer: ZIO[R, Nothing, Any]): ZIO[R, E, A] = f.ensuring(finalizer)
  def mapError[R, E, A, E2](first: ZIO[R, E, A])(f: E => E2): ZIO[R, E2, A] = first.mapError(f)
  def orDie[R, E <: Throwable, A](first: ZIO[R, E, A]): ZIO[R, Nothing, A] = first.orDie
}

implicit val zioMonadSequence: MonadSequence[ZIO] = new MonadSequence[ZIO] {
  def foreach[R, E, A, B, Collection[+Element] <: Iterable[Element]](
      in: Collection[A]
  )(f: A => ZIO[R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ZIO[R, E, Collection[B]] =
    ZIO.foreach(in)(f)
}

implicit val zioMonadSequenceParallel: MonadSequenceParallel[ZIO] = new MonadSequenceParallel[ZIO] {
  def foreachPar[R, E, A, B, Collection[+Element] <: Iterable[Element]](
      in: Collection[A]
  )(f: A => ZIO[R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ZIO[R, E, Collection[B]] =
    ZIO.foreachPar(in)(f)
}
