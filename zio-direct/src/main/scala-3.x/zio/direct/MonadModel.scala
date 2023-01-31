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

// todo in the Scala 3 version can't type this way because it will widen so just do do a type annotation (i.e. `: MonadModel[ZIO]`)
given zioMonadModel: MonadModel[ZIO] with {
  // TODO make this a standard model since same for ZStream etc...
  import MonadShape.Variance._
  import MonadShape.Letter._
  type Variances = MonadShape.Variances3[Contravariant, Covariant, Covariant]
  type Letters = MonadShape.Letters3[R, E, A]
}

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
