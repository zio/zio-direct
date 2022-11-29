package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio.direct.core.util.debug.PrintMac
import zio._
import zio.direct.core.metaprog.Collect
import zio.direct.core.metaprog.Verify
import zio.direct.Dsl.Params
import zio.direct.core.util.Messages

object UnsafeSpec extends DeferRunSpec {
  val spec = suite("UnsafeSpec")(
    suite("Correct ZIO.attempt wrapping of IR.Block statements") {
      test("Single Term IR.Block") {
        val out =
          defer {
            val a = ZIO.succeed(123).run
            throwFoo()
            val b = ZIO.succeed(456).run
            (a, b)
          }
        assertZIO(out.exit)(dies(isSubtype[FooError](anything)))
      }
      +
      test("Single Term IR.Block - Unsafe") {
        val out =
          defer {
            unsafe {
              val a = ZIO.succeed(123).run
              throwFoo()
              val b = ZIO.succeed(456).run
              (a, b)
            }
          }
        assertZIO(out.exit)(fails(isSubtype[FooError](anything)))
      }
      +
      test("ValDef IR.Block - Unsafe") {
        val out =
          defer {
            unsafe {
              val a = ZIO.succeed(123).run
              val v = wantStringThrowFoo()
              val b = ZIO.succeed(456).run
              (a, b)
            }
          }
        assertZIO(out.exit)(fails(isSubtype[FooError](anything)))
      }
      +
      test("Last Statement in impure Block - Unsafe") {
        val out =
          defer {
            unsafe {
              val a = ZIO.succeed(123).run
              wantStringThrowFoo()
            }
          }
        assertZIO(out.exit)(fails(isSubtype[FooError](anything)))
      }
      +
      test("Last Statement in pure Block - Unsafe") {
        val out =
          defer {
            unsafe {
              val a = ZIO.succeed(123).run
              wantStringThrowFoo()
            }
          }
        assertZIO(out.exit)(fails(isSubtype[FooError](anything)))
      }
    },
    suite("Forbidden Constructs") {
      test("IR.Parallel Constructs not allowed in Unsafe (single-effect)") {
        runLiftFailMsg(Messages.UnsafeNotAllowedParallel) {
          """
          unsafe {
            123 + runBlock(ZIO.succeed(456)) + 789
            val v = ZIO.succeed(123).run
            789 + v
          }
          """
        }
      }
      +
      test("IR.Parallel Constructs not allowed in Unsafe (multi-effect)") {
        runLiftFailMsg(Messages.UnsafeNotAllowedParallel) {
          """
          unsafe {
            runBlock(ZIO.succeed(123)) + runBlock(ZIO.succeed(456))
            val v = ZIO.succeed(123).run
            789 + v
          }
          """
        }
      }
    }
  )
}
