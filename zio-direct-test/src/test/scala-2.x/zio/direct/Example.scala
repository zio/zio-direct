package zio.direct

import zio.direct.examples.RunNow
import zio.ZIO
import zio.direct.{run => runBlock}

object Example {
  def main(args: Array[String]): Unit = { // // // // //
    // ZIO.succeed(123).asInstanceOf[ZIO[_, _, _]].catchSome {
    //   case x: Throwable => ZIO.succeed("123")
    // }

    //

    //

    def out = { //
      defer {
        val (a, b) = (runBlock(defer(1)), runBlock(defer(2)))
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
