package zio.direct.future

import zio.direct.DeferRunSpec
import zio.test._
import zio.test.Assertion._
import zio.direct.Use
import zio.direct.core.util.Messages
import scala.annotation.nowarn
import zio.direct.DeferRunSpec
import zio.stream.ZStream
import zio.Chunk
import scala.concurrent.Future
import zio.ZIO
import scala.concurrent.ExecutionContext
import scala.concurrent.Await

object PureSpec extends DeferRunSpec {
  val e = new Exception("blah")
  val e1 = new Exception("blahblah")

  def assertFuture[A](ecToFuture: ExecutionContext => Future[A])(assertion: Assertion[A]): ZIO[Any, Throwable, TestResult] =
    assertZIO(ZIO.fromFuture(ecToFuture(_)))(assertion)

  val spec = suite("VariaSpec")(
    test("Simple Sequence") {
      def out(implicit ec: ExecutionContext) =
        defer {
          val a = Future(1)
          val b = Future("foo")
          (a.run, b.run)
        }
      assertIsType[Future[(Int, String)]](out(???)) andAssert
        assertFuture(out)(equalTo((1, "foo")))
    },
    test("Impure/Impure If-statement") {
      def out(implicit ec: ExecutionContext) =
        defer {
          if (Future[Int](2).run == 2)
            val v = Future("foo").run
            v
          else
            val v = Future("bar").run
            v
        }
      assertFuture(out)(equalTo("foo"))
    },
    test("Impure/Impure Pat-match") {
      def out(implicit ec: ExecutionContext) =
        defer {
          Future("a").run match {
            case "a" => Future(1).run
            case "b" => Future(2).run
          }
        }
      assertFuture(out)(equalTo(1))
    },
    // test("Try/Catch caught") {
    //   def out(implicit ec: ExecutionContext) =
    //     defer.info {
    //       try {
    //         val num = Future(1).run
    //         if (num == 1) {
    //           throw new FooError
    //         } else {
    //           num
    //         }
    //       } catch {
    //         case _: Throwable => Future(18).run
    //       }
    //     }

    //   // assertFuture(out)(equalTo(18))

    //   assert(Await.result(out(scala.concurrent.ExecutionContext.global), scala.concurrent.duration.Duration.Inf))(equalTo(18))
    // },
    test("Try/Catch NOT caught") {
      val fooError = new FooError
      def out(implicit ec: ExecutionContext) =
        defer {
          try {
            val num = Future(3).run
            if (num == 3) {
              throw fooError
            } else {
              num
            }
          } catch {
            case _: BarError => Future(3).run
          }
        }
      assertZIO(ZIO.fromFuture(out).exit)(fails(equalTo(fooError)))
    }
    // test("Throw-fail") {
    //   val fooError = new FooError
    //   def out(implicit ec: ExecutionContext) =
    //     defer(Use.withNoCheck) {
    //       throw fooError
    //     }
    //   assert(out.runAll(init))(equalTo(
    //     (Chunk(), Left(zio.prelude.fx.Cause(fooError)))
    //   ))
    // },
    // test("List Impure, body Impure") {
    //   var v = 1
    //   def out(implicit ec: ExecutionContext) =
    //     defer(Use.withLenientCheck) {
    //       for (i <- Future(List(1, 2, 3)).run) {
    //         Future(v += i).run
    //       }
    //     }
    //   // assert(out.provideState(init).run) doesn't run anything, possibly because the after the .run it's a Unit-type
    //   out.provideState(init).run
    //   assert(v)(equalTo(7))
    // }
  )
}
