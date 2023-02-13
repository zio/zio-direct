package zio.direct

import zio.ZIO

import MonadShape.Variance._
import MonadShape.Letter._
import zio.CanFail
import zio.IsSubtypeOfError

object ZioMonad {
  type ZioMonadModel = MonadModel {
    type Variances = MonadShape.Variances3[Contravariant, Covariant, Covariant]
    type Letters = MonadShape.Letters3[R, E, A]
    type IsFallible = true
  }

// TODO summon better traces i.e. from the non-deferred code?
  val zioMonadSuccess: MonadSuccess[ZIO] = new MonadSuccess[ZIO] {
    def unit[A](a: => A): ZIO[Any, Nothing, A] = ZIO.succeed[A](a)
    def map[R, E, A, B](first: ZIO[R, E, A])(andThen: A => B): ZIO[R, E, B] = first.map[B](andThen)
    def flatMap[R, E, A, B](first: ZIO[R, E, A])(andThen: A => ZIO[R, E, B]): ZIO[R, E, B] = first.flatMap[R, E, B](andThen)
    def flatten[R, E, A, R1 <: R, E1 >: E](first: ZIO[R, E, ZIO[R1, E1, A]]): ZIO[R1, E1, A] = first.flatten
  }

  val zioMonadFallible: MonadFallible[ZIO] = new MonadFallible[ZIO] {
    def fail[E](e: => E): ZIO[Any, E, Nothing] = ZIO.fail(e)
    def attempt[A](a: => A): ZIO[Any, Throwable, A] = ZIO.attempt[A](a)
    def catchSome[R, E, A](first: ZIO[R, E, A])(andThen: PartialFunction[E, ZIO[R, E, A]]): ZIO[R, E, A] = first.catchSome[R, E, A](andThen)(CanFail, summon[zio.Trace])
    def ensuring[R, E, A](f: ZIO[R, E, A])(finalizer: ZIO[R, Nothing, Any]): ZIO[R, E, A] = f.ensuring(finalizer)(summon[zio.Trace])
    def mapError[R, E, A, E2](first: ZIO[R, E, A])(f: E => E2): ZIO[R, E2, A] = first.mapError(f)(CanFail, summon[zio.Trace])
    def orDie[R, E <: Throwable, A](first: ZIO[R, E, A]): ZIO[R, Nothing, A] = first.orDie
  }

  val zioMonadSequence: MonadSequence[ZIO] = new MonadSequence[ZIO] {
    def foreach[R, E, A, B, Collection[+Element] <: Iterable[Element]](
        in: Collection[A]
    )(f: A => ZIO[R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ZIO[R, E, Collection[B]] =
      ZIO.foreach(in)(f)
  }

  val zioMonadSequenceParallel: MonadSequenceParallel[ZIO] = new MonadSequenceParallel[ZIO] {
    def foreachPar[R, E, A, B, Collection[+Element] <: Iterable[Element]](
        in: Collection[A]
    )(f: A => ZIO[R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ZIO[R, E, Collection[B]] =
      ZIO.foreachPar(in)(f)
  }
}
