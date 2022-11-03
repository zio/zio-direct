package zio.asyncawait

import zio.test._
import zio.asyncawait.core.util.debug.PrintMac

object DefSpec extends AsyncAwaitSpec {
  def spec = suite("DefSpec") (
    suite("pure") {
      test("no params") {
        runLiftTest(3) {
          def a = 2
          await(async(1)) + a
        }
      }
      +
      test("one param") {
        runLiftTest(3) {
          def a(i: Int) = i + 1
          await(async(a(1))) + 1
        }
      }
      +
      test("multiple params") {
        runLiftTest(4) {
          def a(i: Int, s: String) = i + s.toInt
          await(async(a(1, "2"))) + a(0, "1")
        }
      }
      +
      test("multiple param groups") {
        runLiftTest(4) {
          def a(i: Int)(s: String) = i + s.toInt
          await(async(a(1)("2"))) + a(0)("1")
        }
      }
      +
      test("nested") {
        runLiftTest(5) {
          def a(i: Int) = i + 1
          def b(s: String) = s.toInt + a(2)
          b("1") + 1
        }
      }
    },
    suite("unlifted") {
      test("no params") {
        runLiftFail {
          """
          def a = await(async(2))
          await(async(1)) + a
          """
        }
      }
      +
      test("one param") {
        runLiftFail {
          """
          def a(i: Int) = await(async(i + 1))
          a(1) + 1
          """
        }
      }
      +
      test("multiple params") {
        runLiftFail {
          """
          runLiftTest(4) {
            def a(i: Int, s: String) = await(async(i)) + s.toInt
            a(1, "2") + await(async(1))
          }
          """
        }
      }
      +
      test("multiple param groups") {
        runLiftFail {
          """
          def a(i: Int)(s: String) = await(async(i)) + await(async(s.toInt))
          a(1)("2") + a(0)("1")
          """
        }
      }
      +
      test("multiple methods") {
        runLiftFail {
          """
          def a(i: Int) = await(async(i + 1))
          def b(s: String) = await(async(s.toInt)) + a(2)
          b("1") + await(async(1))
          """
        }
      }
      +
      test("nested") {
        runLiftFail {
          """
          def a1(s: String) = {
            def a2(i: Int) = await(async(i)) + 1
            a2(s.toInt)
          }
          a1("1")
          """
        }
      }
      +
      test("nested object") {
        runLiftFailMsg("object A") {
          """
          object A {
            def a(i: Int) = await(async(i + 1))
          }
          A.a(1) + 1
          """
        }
      }
      +
      test("nested class - pure") {
        runLiftTest(4) {
          def awaitInt(i: Int) = async(i + 1)
          class A {
            def a(i: Int) = awaitInt(i)
          }
          // TODO test that should fail if foo is a def
          val foo = await((new A).a(1)) + 1
          foo + 1
        }
      }
      +
      test("nested class - pure") {
        runLiftFail {
          """
          def awaitInt(i: Int) = await(async(i + 1))
          class A {
            def a(i: Int) = async(awaitInt(i))
          }
          await((new A).a(1)) + "blah"
          """
        }
      }
      +
      test("nested trait") {
        runLiftFailMsg("trait A") {
          """
          trait A {
            def a(i: Int) = await(async(i + 1))
          }
          (new A {}).a(1) + 1
          """
        }
      }
      +
      test("recursive") {
        runLiftFail {
          """
          def a(i: Int): Int = if (await(async(i)) == 0) 0 else a(i - 1)
          a(10)
          """
        }
      }
    }
  )
}