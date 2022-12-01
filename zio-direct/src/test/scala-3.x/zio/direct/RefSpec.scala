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

object RefSpec extends DeferRunSpec {
  val spec = suite("RefSpec") {
    suite("Refs should work") {
      test("multiple conditions with refs") {
        def isRefY(str: String) = str == "refY"
        runLiftTest("A") {
          val x = Ref.make("refX").run
          val y = Ref.make("refY").run

          if (isRefY(y.get.run)) x.set("refX2").run

          if (x.get.run == "refX2") "A"
          else if (x.get.run == "refX") "B"
          else "C"
        }
      }
    }
  }
}
