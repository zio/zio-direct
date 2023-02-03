package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio.ZIO
import scala.collection.mutable.ArrayBuffer

object IfSpec extends DeferRunSpec {

  val spec = suite("IfSpec")(
    suite("unlifted condition / ifelse")(
      test("pure / pure") {
        runLiftTest(2) {
          if (runBlock(ZIO.succeed(1)) == 1) 2 else 3
        }
      },
      test("pure / impure") {
        runLiftTest(2) {
          if (runBlock(ZIO.succeed(1)) == 1) runBlock(ZIO.succeed(2)) else 3
        }
      },
      test("impure / pure") {
        runLiftTest(2) {
          if (runBlock(ZIO.succeed(1)) == 1) 2 else runBlock(ZIO.succeed(3))
        }
      },
      test("impure / impure") {
        runLiftTest(3) {
          if (runBlock(ZIO.succeed(1)) == 2) runBlock(ZIO.succeed(2)) else runBlock(ZIO.succeed(3))
        }
      }
    ),
    suite("pure condition / ifelse")(
      test("pure / pure") {
        runLiftTest(2) {
          if (1 == 1) 2 else 3
        }
      },
      test("pure / impure") {
        runLiftTest(2) {
          if (1 == 1) runBlock(ZIO.succeed(2)) else 3
        }
      },
      test("impure / pure") {
        runLiftTest(2) {
          if (1 == 1) 2 else runBlock(ZIO.succeed(3))
        }
      },
      test("impure / impure") {
        runLiftTest(3) {
          if (1 == 2) runBlock(ZIO.succeed(2)) else runBlock(ZIO.succeed(3))
        }
      }
    )
  )
}
