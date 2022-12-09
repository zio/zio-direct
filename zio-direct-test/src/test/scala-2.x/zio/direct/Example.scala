package zio.direct

import zio.ZIO
//import zio.direct.core.util.debug.PrintMac
import zio.direct.examples.RunNow
//import zio.direct.{run => runBlock}
import ZIO.{succeed => suc}

object Example {
  def main(args: Array[String]): Unit = { // // // //
    def out = {
      val i = defer(123)
      defer.verbose {
        val v = run(ZIO.succeed(123)) + 1
        // val b = run(ZIO.succeed(789))
        // a + b
        v
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
