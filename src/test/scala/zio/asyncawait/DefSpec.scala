package zio.asyncawait

import zio.test._

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
        runLiftTest(3) {
          def a = await(async(2))
          await(async(1)) + a
        }
      }
      +
      test("one param") {
        runLiftTest(3) {
          def a(i: Int) = await(async(i + 1))
          a(1) + 1
        }
      }
      +
      test("multiple params") {
        runLiftTest(4) {
          def a(i: Int, s: String) = await(async(i)) + s.toInt
          a(1, "2") + await(async(1))
        }
      }
      +
      test("multiple param groups") {
        runLiftTest(4) {
          def a(i: Int)(s: String) = await(async(i)) + await(async(s.toInt))
          a(1)("2") + a(0)("1")
        }
      }
      +
      test("multiple methods") {
        runLiftTest(5) {
          def a(i: Int) = await(async(i + 1))
          def b(s: String) = await(async(s.toInt)) + a(2)
          b("1") + await(async(1))
        }
      }
      +
      test("nested") {
        runLiftTest(2) {
          def a1(s: String) = {
            def a2(i: Int) = await(async(i)) + 1
            a2(s.toInt)
          }
          a1("1")
        }
      }
      +
      test("nested object") {
        runLiftTest(3) {
          object A {
            def a(i: Int) = await(async(i + 1))
          }
          A.a(1) + 1
        }
      }
      +
      test("nested class") {
        runLiftTest(3) {
          class A {
            def a(i: Int) = await(async(i + 1))
          }
          (new A).a(1) + 1
        }
      }
      +
      test("nested trait") {
        runLiftTest(3) {
          trait A {
            def a(i: Int) = await(async(i + 1))
          }
          (new A {}).a(1) + 1
        }
      }
      +
      test("recursive") {
        runLiftTest(0) {
          def a(i: Int): Int = if (await(async(i)) == 0) 0 else a(i - 1)
          a(10)
        }
      }
    }
  )
}