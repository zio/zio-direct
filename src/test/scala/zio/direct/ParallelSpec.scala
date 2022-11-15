package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio.direct.core.util.debug.PrintMac
import zio._
import ZIO._
import zio.direct.Dsl.Params
import zio.direct.core.metaprog.Verify

object ParallelSpec extends AsyncAwaitSpec {

  val spec = suite("ParallelSpec")(
    suite("Multi Await Tests") {
      test("Inferred Environment Should be Correct") {
        val out = defer.info {
          (service[ConfigInt].run, service[ConfigString].run)
        }
        null
      }
    }
  )
}
