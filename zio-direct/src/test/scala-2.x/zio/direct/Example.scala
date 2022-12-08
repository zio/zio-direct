package zio.direct

import zio.ZIO
//import zio.direct.core.util.debug.PrintMac

object Example {
  def main(args: Array[String]): Unit = { // // // // // // // //
    val out =
      defer {
        val a = ZIO.succeed(123).run
        // val b = ZIO.succeed(789).run
        a // + b
      }

    println(out)
    // PrintMac {
    //   val a = ZIO.succeed(123).run
    //   val b = ZIO.succeed(456).run
    //   a + b
    // }
  }
}