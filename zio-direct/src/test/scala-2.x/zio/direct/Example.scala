package zio.direct

import zio.ZIO
//import zio.direct.core.util.debug.PrintMac
import zio.direct.examples.RunNow

object Example {
  def main(args: Array[String]): Unit = {
    val out =
      defer.verbose {
        val a = ZIO.succeed(123).run
        val b = ZIO.succeed(789).run
        a + b
      }

    println("===== Output: " + RunNow(out))
    // PrintMac {
    //   val a = ZIO.succeed(123).run
    //   val b = ZIO.succeed(456).run
    //   a + b
    // }
  }
}
