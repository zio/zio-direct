package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio._
import scala.collection.mutable.ArrayBuffer

object BooleanSpec extends DeferRunSpec {
  // avoids scalac optimizations
  def True = "1".toInt == 1
  def False = "1".toInt == 0
  def NotExpected: Boolean = ???

  val spec = suite("BooleanSpec")(
    suite("&&")(
      test("pure/pure") {
        val out = defer {
          1 == 1 && 2 == 3
        }
        assertZIO(out)(equalTo(False))
      },
      suite("pure/impure")(
        test("True/True") {
          val out = defer {
            True && runBlock(ZIO.succeed(True))
          }
          assertZIO(out)(equalTo(True))
        },
        test("True/False") {
          val out = defer {
            True && runBlock(ZIO.succeed(False))
          }
          assertZIO(out)(equalTo(False))
        },
        test("False/NotExpected") {
          val out = defer {
            False && runBlock(ZIO.succeed(NotExpected))
          }
          assertZIO(out)(equalTo(False))
        }
      ),
      suite("impure/pure")(
        test("True/True") {
          val out = defer {
            runBlock(ZIO.succeed(True)) && True
          }
          assertZIO(out)(equalTo(True))
        },
        test("True/False") {
          val out = defer {
            runBlock(ZIO.succeed(True)) && False
          }
          assertZIO(out)(equalTo(False))
        },
        test("False/NotExpected") {
          val out = defer {
            runBlock(ZIO.succeed(False)) && NotExpected
          }
          assertZIO(out)(equalTo(False))
        }
      ),
      suite("impure/impure")(
        test("True/True") {
          val out = defer {
            runBlock(ZIO.succeed(True)) && runBlock(ZIO.succeed(True))
          }
          assertZIO(out)(equalTo(True))
        },
        test("True/False") {
          val out = defer {
            runBlock(ZIO.succeed(True)) && runBlock(ZIO.succeed(False))
          }
          assertZIO(out)(equalTo(False))
        },
        test("False/NotExpected") {
          val out = defer {
            runBlock(ZIO.succeed(False)) && runBlock(ZIO.succeed(NotExpected))
          }
          assertZIO(out)(equalTo(False))
        }
      )
    ),
    suite("||")(
      test("pure/pure") {
        val out = defer {
          1 == 1 || 2 == 3
        }
        assertZIO(out)(equalTo(True))
      },
      suite("pure/impure")(
        test("False/False") {
          val out = defer {
            False || runBlock(ZIO.succeed(False))
          }
          assertZIO(out)(equalTo(False))
        },
        test("False/True") {
          val out = defer {
            False || runBlock(ZIO.succeed(True))
          }
          assertZIO(out)(equalTo(True))
        },
        test("True/NotExpected") {
          val out = defer {
            True || runBlock(ZIO.succeed(NotExpected))
          }
          assertZIO(out)(equalTo(True))
        }
      ),
      suite("impure/pure")(
        test("False/False") {
          val out = defer {
            runBlock(ZIO.succeed(False)) || False
          }
          assertZIO(out)(equalTo(False))
        },
        test("False/True") {
          val out = defer {
            runBlock(ZIO.succeed(False)) || True
          }
          assertZIO(out)(equalTo(True))
        },
        test("True/NotExpected") {
          val out = defer.verbose {
            runBlock(ZIO.succeed(True)) || NotExpected
          }
          assertZIO(out.debug)(equalTo(True))
        }
      ),
      suite("impure/impure")(
        test("False/False") {
          val out = defer {
            runBlock(ZIO.succeed(False)) || runBlock(ZIO.succeed(False))
          }
          assertZIO(out)(equalTo(False))
        },
        test("False/True") {
          val out = defer {
            runBlock(ZIO.succeed(False)) || runBlock(ZIO.succeed(True))
          }
          assertZIO(out)(equalTo(True))
        },
        test("True/NotExpected") {
          val out = defer {
            runBlock(ZIO.succeed(True)) || runBlock(ZIO.succeed(NotExpected))
          }
          assertZIO(out)(equalTo(True))
        }
      )
    )
  )
}
