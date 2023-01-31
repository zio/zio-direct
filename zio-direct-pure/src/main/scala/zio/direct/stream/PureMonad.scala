// package zio.direct.pure

// import zio.direct._
// import zio.prelude.fx.ZPure
// import zio.ZIO

// import MonadShape.Variance._
// import MonadShape.Letter._

// implicit def pureMonadModel[W, S]: MonadModel[[R, E, A] =>> ZPure[W, S, S, R, E, A]] {
//   type Variances = MonadShape.Variances6[Unused, Unused, Unused, Contravariant, Covariant, Covariant]
//   type Letters = MonadShape.Letters6[Other, Other, Other, R, E, A]
// } = new MonadModel[[R, E, A] =>> ZPure[W, S, S, R, E, A]] {
//   type Variances = MonadShape.Variances6[Unused, Unused, Unused, Contravariant, Covariant, Covariant]
//   type Letters = MonadShape.Letters6[Other, Other, Other, R, E, A]
// }

// implicit def zpureMonadSuccess[W, S]: MonadSuccess[[R, E, A] =>> ZPure[W, S, S, R, E, A]] = new MonadSuccess[[R, E, A] =>> ZPure[W, S, S, R, E, A]] {
//   def unit[A](a: => A): ZPure[W, S, S, Any, Nothing, A] = ZPure.succeed[S, A](a)
//   def map[R, E, A, B](first: ZPure[W, S, S, R, E, A])(andThen: A => B): ZPure[W, S, S, R, E, B] = first.map[B](andThen)
//   def flatMap[R, E, A, B](first: ZPure[W, S, S, R, E, A])(andThen: A => ZPure[W, S, S, R, E, B]): ZPure[W, S, S, R, E, B] = first.flatMap[W, S, R, E, B](andThen)
//   def flatten[R, E, A, R1 <: R, E1 >: E](first: ZPure[W, S, S, R, E, ZPure[W, S, S, R1, E1, A]]): ZPure[W, S, S, R1, E1, A] = first.flatten
// }

// implicit def zpureMonadFallible[W, S]: MonadFallible[[R, E, A] =>> ZPure[W, S, S, R, E, A]] = new MonadFallible[[R, E, A] =>> ZPure[W, S, S, R, E, A]] {
//   def fail[E](e: => E): ZPure[Nothing, Any, Nothing, Any, E, Nothing] = ZPure.fail(e)
//   def attempt[A](a: => A): ZPure[W, S, S, Any, Throwable, A] = ZPure.attempt[S, A](a)
//   def catchSome[R, E, A](first: ZPure[W, S, S, R, E, A])(andThen: PartialFunction[E, ZPure[W, S, S, R, E, A]]): ZPure[W, S, S, R, E, A] = first.catchSome[W, S, S, R, E, A](andThen)
//   def ensuring[R, E, A](f: ZPure[W, S, S, R, E, A])(finalizer: ZPure[W, S, S, R, Nothing, Any]): ZPure[W, S, S, R, E, A] = ??? // f.ensuring(finalizer)
//   def mapError[R, E, A, E2](first: ZPure[W, S, S, R, E, A])(f: E => E2): ZPure[W, S, S, R, E2, A] = first.mapError(f)
//   def orDie[R, E <: Throwable, A](first: ZPure[W, S, S, R, E, A]): ZPure[W, S, S, R, Nothing, A] = ???
// }

// implicit def zstreamMonadSequence[W, S]: MonadSequence[[R, E, A] =>> ZPure[W, S, S, R, E, A]] = new MonadSequence[[R, E, A] =>> ZPure[W, S, S, R, E, A]] {
//   def foreach[R, E, A, B, Collection[+Element] <: Iterable[Element]](
//       in: Collection[A]
//   )(f: A => ZPure[W, S, S, R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ZPure[W, S, S, R, E, Collection[B]] =
//     // TODO Same problem again. Need a different type for the finalization
//     ???
// }

// implicit def zstreamMonadSequencePar[W, S]: MonadSequenceParallel[[R, E, A] =>> ZPure[W, S, S, R, E, A]] = new MonadSequenceParallel[[R, E, A] =>> ZPure[W, S, S, R, E, A]] {
//   def foreachPar[R, E, A, B, Collection[+Element] <: Iterable[Element]](
//       in: Collection[A]
//   )(f: A => ZPure[W, S, S, R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ZPure[W, S, S, R, E, Collection[B]] =
//     // TODO Same problem again. Need a different type for the finalization
//     ???
// }
