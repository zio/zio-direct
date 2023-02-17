package zio.direct

import zio.test._
import zio.test.Assertion._
import zio._

object ErrorSpec extends DeferRunSpec {

  val spec = suite("ErrorSpec")(
    suite("Different Kinds of ways that errors can be thrown")(
      test("Directly thrown error should always go to error channel") {
        val out =
          defer(Use.withNoCheck) {
            throw new FooError
          }
        assertZIO(out.exit)(fails(isSubtype[FooError](anything)))
      },
      test("Throw from a monad") {
        val out =
          defer(Use.withNoCheck) {
            throw (ZIO.succeed(new FooError).run)
          }
        assertZIO(out.exit)(fails(isSubtype[FooError](anything)))
      },
      test("Indirect error function WITH 'unsafe' should be a failure") {
        val out =
          defer(Use.withNoCheck) {
            unsafe { throwFoo() }
          }
        assertZIO(out.exit)(fails(isSubtype[FooError](anything)))
      },
      test("Indirect error function WITHOUT 'unsafe' should be a defect") {
        val out =
          defer(Use.withNoCheck) {
            throwFoo()
          }
        assertZIO(out.exit)(dies(isSubtype[FooError](anything)))
      },
      test("Operations order multiple should be correct") {
        var extern = "foo"
        val out =
          defer(Use.withNoCheck) {
            extern = extern + "bar"
            extern = extern + "baz"
            throwFoo()
            extern = extern + "blin"
          }
        assertZIO(out.exit)(dies(isSubtype[FooError](anything))) andAssert
          assertTrue(extern == "foobarbaz")
      },
      test("Operations order multiple mixed with effects should be correct") {
        // Technically this kind of code is utterly forbidden using zio-direct but it is
        // a useful test to see if blocks are re-composed correctly.
        var extern = "foo"
        val out =
          defer(Use.withNoCheck) {
            extern = extern + "bar"
            ZIO.succeed(extern + "effect").run
            extern = extern + "baz"
            throwFoo()
            extern = extern + "blin"
          }
        assertZIO(out.exit)(dies(isSubtype[FooError](anything))) andAssert
          assertTrue(extern == "foobarbaz")
      },
      // For this whole example to even possibly work incorrectly need to insert an unneeded try element.
      // That would force an outer IR.Block to happen which in one case would cause the throwFoo() error to not
      // even be caught within the zio system.
      test("External error function without 'unsafe' should be a defect - odd case") {
        var extern = 1
        val out =
          defer(Use.withNoCheck) {
            extern = extern + 1
            throwFoo()
            try {
              ZIO.succeed(222).run
            } catch {
              case _: Throwable => 333
            }
          }
        assertZIO(out.exit)(dies(isSubtype[FooError](anything))) andAssert
          assertTrue(extern == 2)
      },
      test("External error function without 'unsafe' should be a defect - odd case + environment") {
        var extern = 1
        val out =
          defer(Use.withNoCheck) {
            extern = extern + 1
            throwFoo()
            try {
              ZIO.service[ConfigInt].map(_.value).run
            } catch {
              case _: Throwable => 333
            }
          }
        assertZIO(out.provide(ZLayer.succeed(ConfigInt(1))).exit)(dies(isSubtype[FooError](anything))) andAssert
          assertTrue(extern == 2)
      },
      test("Need to have correct typing on successful `unsafe` block (issue zio-direct#49)") {
        // To think about. Should we have type widening behavior here?
        def bar() = "bar"
        val out =
          defer {
            val foo = ZIO.succeed("foo").run
            unsafe {
              bar()
            }
          }
        assertIsType[ZIO[Any, Throwable, String]](out) andAssert
          assertZIO(out)(equalTo("bar"))
      }
    )
  )
}
