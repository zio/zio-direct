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

sealed trait MonadShape
object MonadShape {
  sealed trait Variances
  // trait Variances1[T1 <: Variance] extends Variances
  // trait Variances2[T1 <: Variance, T2 <: Variance] extends Variances
  trait Variances3[T1, T2, T3] extends Variances
  // trait Variances4[T1 <: Variance, T2 <: Variance, T3 <: Variance, T4 <: Variance] extends Variances
  // trait Variances5[T1 <: Variance, T2 <: Variance, T3 <: Variance, T4 <: Variance, T5 <: Variance] extends Variances
  // trait Variances6[T1 <: Variance, T2 <: Variance, T3 <: Variance, T4 <: Variance, T5 <: Variance, T6 <: Variance] extends Variances
  // trait Variances7[T1 <: Variance, T2 <: Variance, T3 <: Variance, T4 <: Variance, T5 <: Variance, T6 <: Variance, T7 <: Variance] extends Variances

  sealed trait Letters
  // trait Letters1[T1 <: Letter] extends Letters
  // trait Letters2[T1 <: Letter, T2 <: Letter] extends Letters
  trait Letters3[T1, T2, T3] extends Letters
  // trait Letters4[T1 <: Letter, T2 <: Letter, T3 <: Letter, T4 <: Letter] extends Letters
  // trait Letters5[T1 <: Letter, T2 <: Letter, T3 <: Letter, T4 <: Letter, T5 <: Letter] extends Letters
  // trait Letters6[T1 <: Letter, T2 <: Letter, T3 <: Letter, T4 <: Letter, T5 <: Letter, T6 <: Letter] extends Letters
  // trait Letters7[T1 <: Letter, T2 <: Letter, T3 <: Letter, T4 <: Letter, T5 <: Letter, T6 <: Letter, T7 <: Letter] extends Letters

  sealed trait Variance
  object Variance {
    trait Covariant extends Variance
    case object Covariant extends Covariant
    trait Contravariant extends Variance
    case object Contravariant extends Contravariant
    trait Unused extends Variance
    case object Unused extends Unused
  }

  sealed trait Letter
  object Letter {
    trait R extends Letter
    case object R extends R
    trait E extends Letter
    case object E extends E
    trait A extends Letter
    case object A extends A
    trait Other extends Letter
    case object Other extends Other
  }
}

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
