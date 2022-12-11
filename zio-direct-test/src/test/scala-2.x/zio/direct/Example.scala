package zio.direct

import zio.ZIO
import zio.direct.examples.RunNow

object Example {
  def main(args: Array[String]): Unit = { // // // // // // // // //

    val out =
      defer.tpe {
        val one = ZIO.succeed(123)
        ZIO.succeed(456)
        one
      }

    //

    //

    // def out = {//
    //   defer.verbose {
    //     val (a, b) = (runBlock(defer(1)), runBlock(defer(2)))
    //     a + b
    //   }
    // }

    println("===== Output: " + RunNow(out))
    // PrintMac {
    //   val a = ZIO.succeed(123).run
    //   val b = ZIO.succeed(456).run
    //   a + b
    // }
  }
}
