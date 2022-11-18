package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio.direct.core.util.debug.PrintMac
import zio._
import ZIO._
import zio.direct.core.metaprog.Verify
import zio.direct.Dsl.Params
import zio.direct.Dsl.Internal._
import zio.direct.core.util.Messages

object VariaSpec extends AsyncAwaitSpec {
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
      test("catch await inside await") {
        val msg = Messages.AwaitRemainingAfterTransformer
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
    }
  }
}
