package zio.direct

import zio.direct.examples.RunNow

object Example {
  class FooError extends Exception("foo")
  def throwFoo() = throw new FooError

  def main(args: Array[String]): Unit = { // // // // //

    val out =
      defer(Use.withNoCheck) {
        unsafe { throwFoo() }
      }

    // ZIO.succeed(123).asInstanceOf[ZIO[_, _, _]].catchSome {
    //   case x: Throwable => ZIO.succeed("123")
    // }

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
