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
    }
  )
}
