package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio.direct.core.util.debug.PrintMac
import zio._
import ZIO._
import zio.direct.core.metaprog.Verify
import zio.direct.Dsl.Params

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
    }
  }
}
