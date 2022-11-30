package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio.direct.core.util.debug.PrintMac
import zio._
import ZIO.{unsafe => _, _}
import zio.direct.core.metaprog.Verify
import zio.direct.Dsl.Params
import zio.direct.Dsl.Internal._
import zio.direct.core.util.Messages

object VariaSpec extends DeferRunSpec {
  val spec = suite("VariaSpec") {
    suite("odd placements of defer/run") {
      test("defer in defer") {
        val out =
          defer {
            val v = defer {
              val env = ZIO.service[ConfigInt].run
              env
            }
            v.run.value + 1
          }
        assertZIO(out.provide(ZLayer.succeed(ConfigInt(3))))(equalTo(4))
      }
      +
      test("catch run inside run") {
        val msg = Messages.RunRemainingAfterTransformer
        runLiftFailMsg(msg) {
          """
          val x = succeed(123).run
          val y =
            ignore {
              succeed(456).run
            }
          x + y
          """
        }
      }
      +
      test("disallow mutable collection use") {
        import scala.collection.mutable.ArrayBuffer
        val v = new ArrayBuffer[Int](4)
        runLiftFailMsg(Messages.MutableCollectionDetected) {
          """
          v += 4
          """
        }
      }
      +
      test("disallow mutable collection use - declare but not use") {
        import scala.collection.mutable.ArrayBuffer
        runLiftFailMsg(Messages.MutableCollectionDetected) {
          """
          val v = new ArrayBuffer[Int](4)
          ZIO.succeed(123).run
          """
        }
      }
      +
      test("disallow implicit defs") {
        runLiftFailMsg(Messages.ImplicitsNotAllowed) {
          """
          implicit val x = 123
          x
          """
        }
      }
      +
      test("deconstruct and import") {
        class Blah(val value: Int)
        runLiftTest(4) {
          val (a, a1) = runBlock(ZIO.succeed((1, 2)))
          val blah = new Blah(3)
          import blah._
          val b = runBlock(ZIO.succeed(value))
          a + b
        }
      }
      +
      test("deconstruct and multiple import") {
        class Blah(val value: Int)
        class Blah0(val value0: Int)
        class Blah1(val value1: Int)
        runLiftTest(6) {
          val blah0 = new Blah0(1)
          import blah0._
          val blah1 = new Blah1(2)
          import blah1._
          val (a, a1) = ZIO.succeed((value0, value1)).run
          val blah = new Blah(3)
          import blah._
          val b = ZIO.succeed(value).run
          a + a1 + b
        }
      }
      test("disallow implicit mutable use") {
        var x = 1
        runLiftFailMsg(Messages.MutableAndLazyVariablesNotAllowed) {
          """
          val y = x
          """
        }
      }
      +
      test("disallow implicit mutable use") {
        var x = 1
        runLiftFailMsg(Messages.DeclarationNotAllowed) {
          """
          lazy val x = 123
          """
        }
      }
    }
  }
}
