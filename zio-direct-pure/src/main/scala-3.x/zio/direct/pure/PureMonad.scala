package zio.direct.pure

import zio.direct._
import zio.prelude.fx.ZPure
import zio.prelude.fx
import zio.ZIO

import MonadShape.Variance._
import MonadShape.Letter._
import zio.CanFail

object PureMonad {
  type PureMonadModel = MonadModel {
    type Variances = MonadShape.Variances6[Unused, Unused, Unused, Contravariant, Covariant, Covariant]
    type Letters = MonadShape.Letters6[Other, Other, Other, R, E, A]
  }

  def Success[W, S]: MonadSuccess[[R, E, A] =>> ZPure[W, S, S, R, E, A]] = new MonadSuccess[[R, E, A] =>> ZPure[W, S, S, R, E, A]] {
    def unit[A](a: => A): ZPure[W, S, S, Any, Nothing, A] = ZPure.succeed[S, A](a)
    def map[R, E, A, B](first: ZPure[W, S, S, R, E, A])(andThen: A => B): ZPure[W, S, S, R, E, B] = first.map[B](andThen)
    def flatMap[R, E, A, B](first: ZPure[W, S, S, R, E, A])(andThen: A => ZPure[W, S, S, R, E, B]): ZPure[W, S, S, R, E, B] = first.flatMap[W, S, R, E, B](andThen)
    def flatten[R, E, A, R1 <: R, E1 >: E](first: ZPure[W, S, S, R, E, ZPure[W, S, S, R1, E1, A]]): ZPure[W, S, S, R1, E1, A] = first.flatten
  }

  /**
   * MonadFalliable implementation for ZPure.
   * NOTE: Be sure to always 'plug' the CanFail slots manually. Otherwise when the macro synthesizes
   * calls using catchSome, ensuring, etc... the additional time it will take to "typecheck" the CanFail
   * will horribly slow-down compile-times. Especially if there are various other macros in the
   * same file that are also doing type-checks.
   */
  def Fallible[W, S]: MonadFallible[[R, E, A] =>> ZPure[W, S, S, R, E, A]] = new MonadFallible[[R, E, A] =>> ZPure[W, S, S, R, E, A]] {
    def fail[E](e: => E): ZPure[Nothing, Any, Nothing, Any, E, Nothing] = ZPure.fail(e)
    def attempt[A](a: => A): ZPure[W, S, S, Any, Throwable, A] = ZPure.attempt[S, A](a)
    def catchSome[R, E, A](first: ZPure[W, S, S, R, E, A])(andThen: PartialFunction[E, ZPure[W, S, S, R, E, A]]): ZPure[W, S, S, R, E, A] =
      first.catchSome[W, S, S, R, E, A](andThen)(CanFail)

    def ensuring[R, E, A](f: ZPure[W, S, S, R, E, A])(finalizer: ZPure[W, S, S, R, Nothing, Any]): ZPure[W, S, S, R, E, A] =
      f.foldCauseM(
        (cause: fx.Cause[E]) => finalizer.flatMap(_ => ZPure.failCause(cause)),
        success => finalizer.flatMap(_ => ZPure.succeed(success))
      )(CanFail)

    def mapError[R, E, A, E2](first: ZPure[W, S, S, R, E, A])(f: E => E2): ZPure[W, S, S, R, E2, A] = first.mapError(f)(CanFail)
    def orDie[R, E <: Throwable, A](first: ZPure[W, S, S, R, E, A]): ZPure[W, S, S, R, Nothing, A] =
      first.foldCauseM(
        (cause: fx.Cause[E]) => throw cause.first,
        success => ZPure.succeed(success)
      )(CanFail)
  }

  def Sequence[W, S]: MonadSequence[[R, E, A] =>> ZPure[W, S, S, R, E, A]] = new MonadSequence[[R, E, A] =>> ZPure[W, S, S, R, E, A]] {
    def foreach[R, E, A, B, Collection[+Element] <: Iterable[Element]](
        in: Collection[A]
    )(f: A => ZPure[W, S, S, R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ZPure[W, S, S, R, E, Collection[B]] =
      ZPure.forEach((in: Iterable[A]))(f).map(col => bf.fromSpecific(in)(col))
  }

  def SequencePar[W, S]: MonadSequenceParallel[[R, E, A] =>> ZPure[W, S, S, R, E, A]] = new MonadSequenceParallel[[R, E, A] =>> ZPure[W, S, S, R, E, A]] {
    def foreachPar[R, E, A, B, Collection[+Element] <: Iterable[Element]](
        in: Collection[A]
    )(f: A => ZPure[W, S, S, R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ZPure[W, S, S, R, E, Collection[B]] =
      Sequence.foreach(in)(f)
  }

  def State[W, S]: MonadState[[R, E, A] =>> ZPure[W, S, S, R, E, A], S] = new MonadState[[R, E, A] =>> ZPure[W, S, S, R, E, A], S] {
    override def set(s: S): ZPure[W, S, S, Any, Nothing, Unit] = ZPure.set(s)
    override def get: ZPure[W, S, S, Any, Nothing, S] = ZPure.get[S]
  }

  def Log[W, S]: MonadLog[[R, E, A] =>> ZPure[W, S, S, R, E, A], W] = new MonadLog[[R, E, A] =>> ZPure[W, S, S, R, E, A], W] {
    def log(w: W): ZPure[W, S, S, Any, Nothing, Unit] = ZPure.log[S, W](w)
  }
}
