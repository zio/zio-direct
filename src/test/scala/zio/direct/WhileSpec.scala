package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio.direct.core.util.debug.PrintMac
import zio._
import ZIO._
import zio.direct.core.metaprog.Verify
import zio.direct.Dsl.Params

object WhileSpec extends AsyncAwaitSpec {
  val spec = suite("WhileSpec") {
    suite("run condition") {
      test("pure body") {
        val out =
          defer(Params(Verify.Lenient)) {
            var i = 0
            while (runBlock(succeed(i)) < 3)
              succeed(i += 1).run
            i
          }
        assertZIO(out)(equalTo(3))
      }
      +
      test("impure body") {
        val out =
          defer(Params(Verify.Lenient)) {
            var i = 0
            while (runBlock(succeed(i)) < 3)
              val add = succeed(1).run
              succeed(i += add).run
            i
          }
        assertZIO(out)(equalTo(3))
      }
    }
    +
    suite("pure condition") {
      test("pure body") {
        val out =
          defer(Params(Verify.Lenient)) {
            var i = 0
            while (i < 3)
              succeed(i += 1).run
            i
          }
        assertZIO(out)(equalTo(3))
      }
      +
      test("using ref") {
        val out =
          defer {
            val i = Ref.make(0).run
            while (i.get.run < 3)
              i.getAndUpdate(i => i + 1).run
            i.get.run
          }
        assertZIO(out)(equalTo(3))
      }
    }
  }
}
