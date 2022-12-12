package zio.direct

import zio.ZIO
import zio.direct.examples.RunNow

object Example {
  def main(args: Array[String]): Unit = { // // // // //

    val out =
      defer(Use.withLenientCheck) {
        def getInt(i: Int) = i
        class A {
          def a(i: Int) = getInt(i)
        }
        val foo = ZIO.succeed((new A).a(1)).run + 1
        foo + 1
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
