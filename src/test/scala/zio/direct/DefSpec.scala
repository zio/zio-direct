package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.direct.core.util.debug.PrintMac
import zio.ZIO

object DefSpec extends AsyncAwaitSpec {
  def spec = suite("DefSpec")(
    suiteAll("lenient mode") {
      suiteAll("pure") {
        test("no params") {
          runLiftTestLenient(3) {
            def a = 2
            runBlock(defer(1)) + a
          }
        }
        test("no awaits in functions, even in lenient mode") {
          runLiftFailLenientMsg("In Lenient mode") {
            """
            def a = runBlock(ZIO.succeed(2))
            runBlock(defer(1)) + a
            """
          }
        }
        test("no nested trait, even in lenient mode") {
          runLiftFailLenientMsg("In Lenient mode") {
            """
            trait A {
              def a(i: Int) = runBlock(defer(i + 1))
            }
            (new A {}).a(1) + 1
            """
          }
        }
        test("one param") {
          runLiftTestLenient(3) {
            def a(i: Int) = i + 1
            runBlock(defer(a(1))) + 1
          }
        }
        test("multiple params") {
          runLiftTestLenient(4) {
            def a(i: Int, s: String) = i + s.toInt
            runBlock(defer(a(1, "2"))) + a(0, "1")
          }
        }
        test("multiple param groups") {
          runLiftTestLenient(4) {
            def a(i: Int)(s: String) = i + s.toInt
            runBlock(defer(a(1)("2"))) + a(0)("1")
          }
        }
        test("nested") {
          runLiftTestLenient(5) {
            def a(i: Int) = i + 1
            def b(s: String) = s.toInt + a(2)
            b("1") + 1
          }
        }
        test("nested class - pure") {
          runLiftTestLenient(3) {
            def getInt(i: Int) = i
            class A {
              def a(i: Int) = getInt(i)
            }
            val foo = ZIO.succeed((new A).a(1)).run + 1
            foo + 1
          }
        }
        test("nested class - pure - with nested defer") {
          runLiftTestLenient(4) {
            def getInt(i: Int) = defer(i + 1)
            class A {
              def a(i: Int) = getInt(i)
            }
            // TODO test that should fail if foo is a def
            val foo = runBlock((new A).a(1)) + 1
            foo + 1
          }
        }
      }
    },
    suite("unlifted") {
      val errorMsgContains = "not allowed inside of defer blocks"
      test("def not allowed") {
        runLiftFailMsg(errorMsgContains) {
          """
          def a = 2
          runBlock(defer(1)) + a
          """
        }
      }
      +
      test("nested class - pure - not allowed") {
        runLiftFailMsg(errorMsgContains) {
          """
          class A {
            def a(i: Int) = i + 1
          }
          (new A).a(1) + 456
          """
        }
      }
      +
      test("no nested trait") {
        runLiftFail {
          """
          trait A {
            def a(i: Int) = i + 1
          }
          (new A {}).a(1) + 1
          """
        }
      }
      +
      test("nested class - pure but using defer - not allowed") {
        runLiftFailMsg(errorMsgContains) {
          """
          def awaitInt(i: Int) = runBlock(defer(i + 1))
          class A {
            def a(i: Int) = defer(awaitInt(i))
          }
          runBlock((new A).a(1)) + "blah"
          """
        }
      }
    }
  )
}
