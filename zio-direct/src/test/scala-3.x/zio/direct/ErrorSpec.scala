package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio.direct.core.util.debug.PrintMac
import zio._
import zio.direct.core.metaprog.Collect
import zio.direct.core.metaprog.Verify
import zio.direct.Dsl.Params

object ErrorSpec extends AsyncAwaitSpec {

  val spec = suite("ErrorSpec")(
    suite("Different Kinds of ways that errors can be thrown") {
      test("Directly thrown error should always go to error channel") {
        val out =
          defer(Params(Verify.None)) {
            throw new FooError
          }
        assertZIO(out.exit)(fails(isSubtype[FooError](anything)))
      }
      +
      test("Indirect error function WITH 'unsafe' should be a failure") {
        val out =
          defer(Params(Verify.None)) {
            unsafe { throwFoo() }
          }
        assertZIO(out.exit)(fails(isSubtype[FooError](anything)))
      }
      +
      test("Indirect error function WITHOUT 'unsafe' should be a defect") {
        val out =
          defer(Params(Verify.None)) {
            throwFoo()
          }
        assertZIO(out.exit)(dies(isSubtype[FooError](anything)))
      }
      +
      // For this whole example to even possibly work incorrectly need to insert an unneeded try element.
      // That would force an outer IR.Block to happen which in one case would cause the throwFoo() error to not
      // even be caught within the zio system.
      test("External error function without 'unsafe' should be a defect - odd case") {
        var extern = 1
        val out =
          defer(Params(Verify.None)) {
            extern = extern + 1
            throwFoo()
            try {
              ZIO.succeed(222).run
            } catch {
              case _ => 333
            }
          }
        assertZIO(out.exit)(dies(isSubtype[FooError](anything))) *>
          assertTrue(extern == 2)
      }
      +
      test("External error function without 'unsafe' should be a defect - odd case + environment") {
        var extern = 1
        val out =
          defer(Params(Verify.None)) {
            extern = extern + 1
            throwFoo()
            try {
              ZIO.service[ConfigInt].map(_.value).run
            } catch {
              case _ => 333
            }
          }
        assertZIO(out.provide(ZLayer.succeed(ConfigInt(1))).exit)(dies(isSubtype[FooError](anything))) *>
          assertTrue(extern == 2)
      }
    }
  )
}
