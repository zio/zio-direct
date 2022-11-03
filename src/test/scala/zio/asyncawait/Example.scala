package zio.asyncawait

import zio._
import zio.asyncawait.core.util.debug.PrintMac

object Example {
  // def funA():Unit = {
  //   // Works?
  //   // PrintMac( lift {
  //   //   val a = unlift(ZIO.succeed(123).asInstanceOf[Task[Int]])
  //   //   unlift(ZIO.succeed(2).asInstanceOf[Task[Any]])
  //   // })

  //   // PrintMac
  //   // val out =
  //   // (lift {
  //   //   val a = unlift(ZIO.succeed(123).asInstanceOf[Task[Int]])
  //   //   unlift(ZIO.succeed(2).asInstanceOf[Task[Int]])
  //   //   a
  //   // })


  //   class Blah(input: Int) {
  //     def value: Int = input
  //   }
  //   // PrintMac(async {
  //   //   val (a, a1) = await(ZIO.succeed((123, 456)))
  //   //   val blah = new Blah(2)
  //   //   import blah._
  //   //   val b = await(ZIO.succeed(value))
  //   //   a + b
  //   // })

  //   val out =
  //     async {
  //       val (a, a1) = await(ZIO.succeed((123, 456)))
  //       val blah = new Blah(2)
  //       import blah._
  //       val b = await(ZIO.succeed(value))
  //       a + b
  //     }

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

  // def funB():Unit = {

  //   class Blah(input: Int):
  //     def value: Int = input
  //   class Blah0(input: Int):
  //     def value0: Int = input
  //   class Blah1(input: Int):
  //     def value1: Int = input

  //   val out =
  //     async {
  //       val blah0 = new Blah0(123)
  //       import blah0._
  //       val blah1 = new Blah1(456)
  //       import blah1._
  //       val (a, a1) = await(ZIO.succeed((blah0.value0, blah1.value1)))
  //       val blah = new Blah(2)
  //       import blah._
  //       val b = await(ZIO.succeed(value))
  //       a + a1 + b
  //     }

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

  // def funC(): Unit = {
  //   val out =
  //     PrintMac.passthrough(async {
  //       val (a, b) = (await(async(1)), await(async(2)))
  //       a + b
  //     })

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

  // def funD(): Unit = {
  //   val i = async(1)
  //   val j = async(2)

  //   PrintMac(async {
  //       val v = await(i)
  //       v + await(j)
  //     })

  //   val out =
  //     async {
  //       val v = await(i)
  //       v + await(j)
  //     }



  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

  // def funE(): Unit = {
  //   val out =
  //     async {
  //       def a(i: Int, s: String) = i + s.toInt
  //       await(async(a(1, "2"))) + a(0, "1")
  //     }

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

  // def funF(): Unit = {
  //   val out = (async {
  //     def a = async("foo")
  //     await(a) + "bar"
  //   })

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

  // def funG(): Unit = {
  //   // PrintMac(async {
  //   //   def blah(b: String) = await(async(b))
  //   //   blah("foo") + "bar"
  //   // })

  //   val out = (async {
  //     def blah(b: String)(c: String) = await(async(b + c))
  //     blah("foo")("bar") + "baz"
  //   })

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

  def funH(): Unit = {
    PrintMac(async {
      if (await(ZIO.succeed("1".toInt == 1)) && await(ZIO.succeed("2".toInt == 2)))
        "foo"
      else
        "bar"
    })

    // val out = (async {
    //   def blah(b: String)(c: String) = await(async(b + c))
    //   blah("foo")("bar") + "baz"
    // })

    // val outRun =
    //   zio.Unsafe.unsafe { implicit unsafe =>
    //     zio.Runtime.default.unsafe.run(out).getOrThrow()
    //   }
    // println("====== RESULT: " + outRun)
  }

  def main(args: Array[String]): Unit = {
    funH() // //
  }
}
