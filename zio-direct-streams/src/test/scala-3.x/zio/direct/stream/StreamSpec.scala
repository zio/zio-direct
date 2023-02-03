package zio.direct.stream

import zio.direct.stream.each
import zio.direct.DeferRunSpec
import zio.test._
import zio.test.Assertion._
import zio.direct.Use
import zio.direct.core.util.Messages
import scala.annotation.nowarn
import zio.direct.DeferRunSpec
import zio.stream.ZStream
import zio.Chunk

object StreamSpec extends DeferRunSpec {

  val e = new Exception("blah")
  val e1 = new Exception("blahblah")

  val spec = suite("VariaSpec")(
    test("Simple Sequence") {
      val out =
        defer {
          val a = ZStream(1, 2, 3)
          val b = ZStream("foo", "bar")
          (a.each, b.each)
        }
      assertIsType[ZStream[Any, Nothing, (Int, String)]](out) andAssert
        assertZIO(out.runCollect)(equalTo(
          Chunk((1, "foo"), (1, "bar"), (2, "foo"), (2, "bar"), (3, "foo"), (3, "bar"))
        ))
    },
    test("Impure/Impure If-statement") {
      val out =
        defer {
          if (ZStream(1, 2).each == 2) ZStream("foo", "bar").each else ZStream("x", "y").each
        }
      assertZIO(out.runCollect)(equalTo(
        Chunk("x", "y", "foo", "bar")
      ))
    },
    test("Impure/Impure Pat-match") {
      val out =
        defer {
          ZStream("a", "b").each match {
            case "a" => ZStream(1, 2).each
            case "b" => ZStream(3, 4).each
          }
        }
      assertZIO(out.runCollect)(equalTo(
        Chunk(1, 2, 3, 4)
      ))
    },
    test("Try/Catch succeed") {
      val out =
        defer {
          try {
            throw (ZStream.succeed(e).each)
          } catch {
            case `e`          => 1
            case _: Throwable => ZStream.succeed(2).each
          }
        }
      assertIsType[ZStream[Any, Exception, Int]](out) andAssert
        assertZIO(out.runCollect)(equalTo(Chunk(1)))
    },
    test("Try/Catch caught") {
      val out =
        defer {
          try {
            val num = ZStream(1, 2, 3, 4).each
            if (num == 3) {
              throw new FooError
            } else {
              num
            }
          } catch {
            case _: FooError => ZStream(33, 44, 55).each
          }
        }
      assertIsType[ZStream[Any, FooError, Int]](out) andAssert
        assertZIO(out.runCollect)(equalTo(Chunk(1, 2, 33, 44, 55)))
    },
    test("Throw-fail") {
      val out =
        defer(Use.withNoCheck) {
          throw new FooError
        }
      assertZIO(out.runCollect.exit)(fails(isSubtype[FooError](anything)))
    },
    test("Throw-die") {
      val out =
        defer(Use.withNoCheck) {
          throwFoo()
        }
      assertZIO(out.runCollect.exit)(dies(isSubtype[FooError](anything)))
    },
    test("List Impure, body Impure") {
      var v = 1
      val out =
        defer(Use.withLenientCheck) {
          for (i <- ZStream(List(1, 2, 3), List(4, 5, 6)).each) {
            ZStream(v += i).each
          }
        }
      assertZIO(out.runCollect)(equalTo(Chunk((), ()))) andAssert
        assert(v)(equalTo(22))
    },
    test("Complex Example Case") {
      val x = ZStream(1, -2, -3)
      val y = ZStream("ab", "cde")
      val out =
        defer {
          val xx = x.each
          xx + (
            if xx > 0 then y.each.length() * x.each
            else y.each.length()
          )
        }
      // the above statement does not actually expand to this but they should be equivalent
      val compare =
        x.flatMap { xx =>
          if (xx > 0) {
            y.flatMap { yEach =>
              x.map { xEach =>
                xx + yEach.length * xEach
              }
            }
          } else {
            y.map { yEach =>
              xx + yEach.length
            }
          }
        }
      val expectedOutput = Chunk(3, -3, -5, 4, -5, -8, 0, 1, -1, 0)
      for {
        a <- out.runCollect
        b <- compare.runCollect
      } yield {
        assert(a)(equalTo(b)) && assert(a)(equalTo(expectedOutput))
      }
    }
  )
}
