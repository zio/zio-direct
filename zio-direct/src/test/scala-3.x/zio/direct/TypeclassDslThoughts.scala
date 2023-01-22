package zio.direct

import zio.ZIO

import scala.annotation._

object TypeclassDslThoughts {

  @implicitNotFound("This data type does not support success")
  trait Successful[F[-_, +_, +_]] { // trait Successful[F[_, _, _]] {
    type ComposeSuccess[A, B] = A | B
    def succeed[A](a: => A): F[Any, Nothing, A]
    def flatMap[R, E, A, B](first: F[R, E, A])(andThen: A => F[R, E, B]): F[R, E, B]
    def map[R, E, A, B](first: F[R, E, A])(map: A => B): F[R, E, B] = flatMap(first)(a => succeed[B](map(a)))
  }
  object Successful {
    implicit val zioSuccessful: Successful[ZIO] =
      new Successful[ZIO] {
        // def succeed[R, E, A](a: => A): ZIO[R, E, A] = ZIO.succeed(a)
        def succeed[A](a: => A): ZIO[Any, Nothing, A] = ZIO.succeed(a)

        def flatMap[R, E, A, B](first: ZIO[R, E, A])(andThen: A => ZIO[R, E, B]): ZIO[R, E, B] = first.flatMap(andThen)
      }
  }

  @implicitNotFound("This data type does not support error effects, including try / catch")
  trait Fallible[F[_, _, _]] {
    type ComposeError[A, B] = A | B

    def fail[E](e: => E): F[Any, E, Nothing]

    // NoSuchElementFound?
    def catchSome[R, E1 <: Throwable, E2 <: Throwable, A](first: F[R, E1, A])(andThen: PartialFunction[E1, F[R, E2, A]]): F[R, E2, A]

    def ensuring[R, E, A](f: F[R, E, A], finalizer: F[R, Nothing, Any]): F[R, E, A]
  }

  @implicitNotFound("This data type does not support environmental effects")
  trait Environmental[F[_, _, _]] {
    type ComposeEnvironment[A, B] = A & B

    def require[R]: F[R, Nothing, R]

    def provide[R, E, A](effect: F[R, E, A], r: R): F[Any, E, A]
  }

  trait Stateful[F[_, -_, +_, +_]] {
    type ComposeState[L, R] = (L, R)

    def get[S]: F[S, Any, Nothing, S]

    def set[S](s: S): F[S, Any, Nothing, Unit]
  }

  @main
  def hello = println("Hello World!")
}
