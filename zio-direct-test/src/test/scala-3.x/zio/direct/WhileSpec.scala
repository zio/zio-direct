package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio.direct.core.util.debug.PrintMac
import zio._
import ZIO._
import zio.direct.core.metaprog.Verify
import zio.direct.Dsl.Params
import scala.collection.mutable.ArrayBuffer

object WhileSpec extends DeferRunSpec {
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
      test("double in tuple - strange case") {
        var i = 0
        val buff1 = new ArrayBuffer[Int]()
        val buff2 = new ArrayBuffer[Int]()
        def incrementA() = {
          i += 1
          buff1.addOne(i)
          i
        }
        def incrementB() = {
          i += 1
          buff2.addOne(i)
          i
        }
        val out =
          defer(Params(Verify.Lenient)) {
            while (i < 3)
              (succeed(incrementA()).run, succeed(incrementB()).run)
            i
          }
        for {
          a <- out
        } yield (
          assert(a)(equalTo(4)) &&
            assert(buff1.toList)(equalTo(List(1, 3))) &&
            assert(buff2.toList)(equalTo(List(2, 4)))
        )
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
