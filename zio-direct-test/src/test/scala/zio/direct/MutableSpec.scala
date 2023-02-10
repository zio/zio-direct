package zio.direct

import zio.test._
import scala.annotation.nowarn
import scala.collection.mutable.ArrayBuffer
import zio.direct.core.util.Messages
import zio.ZIO

object MySpec extends DeferRunSpec {
  val spec = suite("MySpec")(
    suite("mySuite")(
      List(1, 2, 3).map(no =>
        test(s"Test no: $no")(
          assertTrue(true)
        )
      )
    )
  )
}

@nowarn
object MutableSpec extends DeferRunSpec {
  val spec = suite("MutableSpec")(
    suite("Mutable Var")(
      test("in effect") {
        var x = 1
        @nowarn
        val out =
          defer {
            (ZIO.succeed({ x = x + 1; x }).run, ZIO.succeed(x).run)
          }
        assertZIO(out)(Assertion.equalTo((2, 2))) andAssert
          assertTrue(x == 2)
      },
      test("plain not allowed") {
        var x = 1
        runLiftFailMsg(Messages.AssignmentNotAllowed) {
          """
          x = x + 1; x
          """
        }
      },
      test("use of mutable not allowed") {
        var x = 1
        runLiftFailMsg(Messages.MutableAndLazyVariablesNotAllowed) {
          """
          val y = x
          """
        }
      }
    ),
    suite("Mutable Collection")(
      test("in effect") {
        var buff = new ArrayBuffer[Int]()
        buff += 1
        val out =
          defer {
            (ZIO.succeed({ buff.update(0, buff(0) + 1); buff(0) }).run, ZIO.succeed(buff(0)).run)
          }
        assertZIO(out)(Assertion.equalTo((2, 2))) andAssert
          assertTrue(buff(0) == 2)
      },
      test("plain not allowed") {
        var buff = new ArrayBuffer[Int]()
        buff += 1
        runLiftFailMsg(Messages.MutableCollectionDetected("ArrayBuffer")) {
          """
          buff.update(0, buff(0) + 1); buff(0)
          """
        }
      }
    )
  )
}
