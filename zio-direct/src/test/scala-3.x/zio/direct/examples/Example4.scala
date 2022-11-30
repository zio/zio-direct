package zio.direct.examples

import zio.direct._
import zio._

object Example4 {
  def main(args: Array[String]): Unit = {
    val out = defer {
      val a = ZIO.succeed(123).run
      val b = ZIO.succeed(123).run
      (a, b)
    }

    RunNow(out)
  }
}
