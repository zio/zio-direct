package zio.direct

import zio.test._
import zio._

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
