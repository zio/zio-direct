package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio.ZIO
import zio.ZIO.{unsafe => _, _}

object TryExtraSpec extends DeferRunSpec {

  val e = new Exception("blah")
  val e1 = new Exception("blahblah")

  def spec =
    suite("TrySpec")(
      test("not caught error in parallel block - error should be fail-channel") {
        val out = defer {
          try {
            (123, { throw new FooError })
          } catch {
            case `e` => 111
          }
        }
        assertIsType[ZIO[Any, FooError, (Int, Nothing) | 111]](out) andAssert
          assertZIO(out.exit)(fails(isSubtype[FooError](anything)))
      },
      test("not caught error in parallel block (fail-channel)") {
        val out = defer {
          try {
            (123, attempt(throw new FooError).run)
          } catch {
            case `e` => 111
          }
        }
        assertIsType[ZIO[Any, Throwable, (Int, Nothing) | 111]](out) andAssert
          assertZIO(out.exit)(fails(isSubtype[FooError](anything)))
      }
    )
}
