package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import scala.annotation.nowarn

@nowarn("msg=It would fail on the following input")
object PatMatchSpec extends DeferRunSpec {

  val spec = suite("PatMatchSpec")(
    suite("unlifted scrutinee")(
      suite("without guards")(
        test("pure cases") {
          runLiftTest(3) {
            runBlock(defer("b")) match {
              case "a" => 2
              case "b" => 3
            }
          }
        },
        test("pure cases with val") {
          runLiftTest(3) {
            val v = runBlock(defer("b"))
            v match {
              case "a" => 2
              case "b" => 3
            }
          }
        },
        test("pure/impure cases") {
          runLiftTest(2) {
            runBlock(defer("a")) match {
              case "a" => runBlock(defer(2))
              case "b" => 3
            }
          }
        },
        test("impure cases") {
          runLiftTest(3) {
            runBlock(defer("b")) match {
              case "a" => runBlock(defer(2))
              case "b" => runBlock(defer(3))
            }
          }
        }
      ),
      suite("with guards")(
        test("pure cases") {
          runLiftTest(3) {
            runBlock(defer("b")) match {
              case s if s == "a" => 2
              case "b"           => 3
            }
          }
        },
        test("pure/impure cases") {
          runLiftTest(2) {
            runBlock(defer("a")) match {
              case "a"           => runBlock(defer(2))
              case s if s == "b" => 3
            }
          }
        },
        test("impure cases") {
          runLiftTest(2) {
            runBlock(defer("b")) match {
              case s if "1".toInt == 1 => runBlock(defer(2))
              case "b"                 => runBlock(defer(3))
            }
          }
        }
      )
    ),
    suite("pure scrutinee")(
      suite("without guards")(
        test("pure cases") {
          runLiftTest(3) {
            ("b": String) match {
              case "a" => 2
              case "b" => 3
            }
          }
        },
        test("pure/impure cases") {
          runLiftTest(2) {
            runBlock(defer("a")) match {
              case "a" => runBlock(defer(2))
              case "b" => 3
            }
          }
        },
        test("impure cases") {
          runLiftTest(3) {
            ("b": String) match {
              case "a" => runBlock(defer(2))
              case "b" => runBlock(defer(3))
            }
          }
        }
      ),
      suite("with guards")(
        test("pure cases") {
          runLiftTest(3) {
            "b" match {
              case s if s == "a" => 2
              case "b"           => 3
            }
          }
        },
        test("pure/impure cases") {
          runLiftTest(2) {
            runBlock(defer("a")) match {
              case "a"           => runBlock(defer(2))
              case s if s == "b" => 3
            }
          }
        },
        test("impure cases") {
          runLiftTest(2) {
            "b" match {
              case s if "1".toInt == 1 => runBlock(defer(2))
              case "b"                 => runBlock(defer(3))
            }
          }
        }
      )
    ),
    suite("misc")(
      test("val patmatch") {
        runLiftTest(1) {
          val Some(a) = runBlock(defer(Some(1)))
          a
        }

        // "invalid unlifted guard" in pendingUntilFixed {
        //   runLiftTest(2) {
        //     "b" match {
        //       case s if runBlock(defer(true)) => 2
        //       case "b"                     => runBlock(defer(3))
        //     }
        //   }
      }
    )
  )
}
