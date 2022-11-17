package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.direct.core.util.debug.PrintMac

object BlockSpec extends AsyncAwaitSpec {
  def spec =
    suite("BlockSpec")(
      suite("assigned await") {
        test("only") {
          val i = defer(1)
          runLiftTest(1) {
            val v = i.run
            v
          }
        }
        +
        test("followed by pure expression") {
          val i = defer(1)
          runLiftTest(2) {
            val v = i.run
            v + 1
          }
        }
        +
        test("followed by impure expression") {
          val i = defer(1)
          val j = defer(2)
          runLiftTest(3) {
            val v = i.run
            v + j.run
          }
        }
        +
        test("nested") {
          val i = defer(1)
          runLiftTest(3) {
            val v = {
              val r = i.run
              r + 1
            }
            v + 1
          }
        }
      },
      suite("unassigned await") {
        test("only") {
          val i = defer(1)
          runLiftTest(1) {
            i.run
          }
        }
        +
        test("followed by pure expression") {
          val i = defer(1)
          runLiftTest(2) {
            i.run
            2
          }
        }
        +
        test("followed by impure expression") {
          val i = defer(1)
          val j = defer(2)
          runLiftTest(2) {
            i.run
            j.run
          }
        }
      },
      suite("pure expression") {
        test("only") {
          runLiftTest(1) {
            1
          }
        }
        +
        test("followed by pure expression") {
          def a = 1
          runLiftTest(2) {
            a
            2
          }
        }
        +
        test("followed by impure expression") {
          val i = defer(1)
          def a = 2
          runLiftTest(1) {
            a
            i.run
          }
        }
      },
      suite("complex") {
        // test is not working
        test("tuple val pattern") {
          runLiftTest(3) {
            val (a, b) = (runBlock(defer(1)), runBlock(defer(2)))
            a + b
          }
        }
        +
        test("assignment not allowed") {
          val i = defer(1)
          runLiftFail {
            """
            var x = 0
            x += {
              val v = i.run + 2
              v + 1
            }
            x
            """
          }
        }
      }
    )
}
