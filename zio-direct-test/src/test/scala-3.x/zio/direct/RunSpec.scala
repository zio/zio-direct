package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio._
import ZIO.{unsafe => _, _}
import zio.direct.core.util.Messages
import scala.annotation.nowarn

@nowarn
object RunSpec extends DeferRunSpec {

  val spec = suite("RunSpec")(
    test("supports run method without extension syntax") {
      @directRunCall
      def await[T](v: Task[T]): T = ???
      class Blah(val value: Int)
      runLiftTest(1) {
        await(ZIO.succeed(1))
      }
    }
  )
}
