package zio.direct

import zio.ZIO
//import zio.direct.core.util.debug.PrintMac
import zio.direct.examples.RunNow
//import zio.direct.{run => runBlock}
import zio.direct.{run => runBlock}
import zio.direct.core.util.debug.PrintMac

object Example {
  def main(args: Array[String]): Unit = { // // // //
    def out = {
      val i = defer(123)
      defer {
        val a = runBlock(i) + 1
        val b = runBlock(ZIO.succeed(789))
        a + b
      }
    }

    println("===== Output: " + RunNow(out))
    // PrintMac {
    //   val a = ZIO.succeed(123).run
    //   val b = ZIO.succeed(456).run
    //   a + b
    // }
  }
}
