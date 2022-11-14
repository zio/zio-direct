package zio.run

import zio._
import zio.run.core.util.debug.PrintMac
import java.sql.SQLException
import java.io.IOException

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
  //   // PrintMac(defer {
  //   //   val (a, a1) = await(ZIO.succeed((123, 456)))
  //   //   val blah = new Blah(2)
  //   //   import blah._
  //   //   val b = await(ZIO.succeed(value))
  //   //   a + b
  //   // })

  //   val out =
  //     defer {
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
  //     defer {
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
  //     PrintMac.passthrough(defer {
  //       val (a, b) = (await(defer(1)), await(defer(2)))
  //       a + b
  //     })

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

  // def funD(): Unit = {
  //   val i = defer(1)
  //   val j = defer(2)

  //   PrintMac(defer {
  //       val v = await(i)
  //       v + await(j)
  //     })

  //   val out =
  //     defer {
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
  //     defer {
  //       def a(i: Int, s: String) = i + s.toInt
  //       await(defer(a(1, "2"))) + a(0, "1")
  //     }

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

  // def funF(): Unit = {
  //   val out = (defer {
  //     def a = defer("foo")
  //     await(a) + "bar"
  //   })

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

  // def funG(): Unit = {
  //   // PrintMac(defer {
  //   //   def blah(b: String) = await(defer(b))
  //   //   blah("foo") + "bar"
  //   // })

  //   val out = (defer {
  //     def blah(b: String)(c: String) = await(defer(b + c))
  //     blah("foo")("bar") + "baz"
  //   })

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }
  //
  //

  // // // // // // // ..

  // def funH(): Unit = {

  //   // Correct issue but error is misleading
  //   // val out = (defer {
  //   //   if (await({
  //   //     for {
  //   //       env <- ZIO.service[Config]
  //   //       value <- ZIO.succeed(env.value)
  //   //     } yield (value)
  //   //   }) == "blah")
  //   //     "foo"
  //   //   else
  //   //     "barr"
  //   // })

  //   // val out =
  //   //   defer.info {
  //   //     val (x, y) = (await(ZIO.succeed("foo")), await(ZIO.succeed("bar")))
  //   //     val (x1, y1) = (await(ZIO.succeed("foo2" + x)), await(ZIO.succeed("bar2" + y)))
  //   //     x + x1 + y + y1
  //   //   }

  //   // Make a test for this
  //   // val out =
  //   //   defer.verbose {
  //   //     val (x, y) = (await(ZIO.succeed("foo")), await(ZIO.succeed("bar")))
  //   //     val config = await(ZIO.service[Config])
  //   //     x + config.value + y
  //   //   }

  //   // Noisy exception
  //   // val out =
  //   //   defer.info {
  //   //     val tup = (await(ZIO.succeed("foo")), await(ZIO.succeed("bar")))
  //   //     val configValue =
  //   //       await(ZIO.service[Config]) match {
  //   //         case Config(value) => value
  //   //       }
  //   //     tup._1 + config.value + tup._2
  //   //   }

  //   // val out =
  //   //   defer.info {
  //   //     val tup = (await(ZIO.succeed("foo")), await(ZIO.succeed("bar")))
  //   //     val configValue =
  //   //       await(ZIO.service[Config]) match {
  //   //         case Config(value) => await(ZIO.succeed(value))
  //   //       }
  //   //     tup._1 + configValue + tup._2
  //   //   }

  //   val out =
  //     defer {
  //       def a(i: Int, s: String) = await(defer(i)) + s.toInt
  //       a(1, "2") + await(defer(1))
  //     }

  //   val provided = out //.provide(ZLayer.succeed(Config("x")))

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(provided).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

  // def funH(): Unit = { //
  //   val out =
  //     defer.verbose {
  //       val msg =
  //         defer(
  //           try {
  //             //await(ZIO.attempt("foo"))
  //             await(ZIO.attempt { throw new IOException("blah") })
  //           } catch {
  //             case e: IOException => e.getMessage()
  //           } finally {
  //             println("============ Recovering ========")
  //           }
  //         )
  //       val msgResult = await(msg)

  //       await(ZIO.succeed(msgResult))
  //     }

  //   // Bug: moving currTime right before starTime makes a "forward reference"

  //   def currTime(): Double = java.lang.System.currentTimeMillis() //

  //   // val out =
  //   //   defer.verbose {
  //   //     val a = ZIO.sleep(10.seconds).fork.run
  //   //     val b = ZIO.sleep(2.seconds).fork.run
  //   //     lazy val startTime = currTime()
  //   //     ZIO.sleep(4.seconds)
  //   //     zio.Console.printLine(s"Started waiting: ${(currTime() - startTime)/1000d}").run
  //   //     val aResult = await(a.join)
  //   //     zio.Console.printLine(s"A completed: ${(currTime() - startTime)/1000d}").run
  //   //     val bResult = await(b.join)
  //   //     zio.Console.printLine(s"B completed: ${(currTime() - startTime)/1000d}").run
  //   //     (aResult, bResult)
  //   //   }

  //   // def currTime(): Double = java.lang.System.currentTimeMillis()
  //   // val out =
  //   //   defer.verbose {
  //   //     val startTime = currTime()
  //   //     val a = await(ZIO.collectAllPar(Chunk(ZIO.sleep(10.seconds), ZIO.sleep(2.seconds))))
  //   //     await(zio.Console.printLine(s"Completed in: ${(currTime() - startTime)/1000d}"))
  //   //     a
  //   //   }

  //   // val out =
  //   //   defer.info {
  //   //     (await(ZIO.sleep(5.seconds)), await(ZIO.sleep(10.seconds)))
  //   //   }

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

  // while ({ val v = await(i.get) - 2; println(v); v } > 0) { /

//   def funJ() = {

//     val out =
//       defer.verbose {
//         var i = 10
//         (await(ZIO.succeed({ i = i - 1 })), await(ZIO.succeed({ i = i - 2 })))
//       }

// // (TODO this should also give a warning)
// // emphasize "for effect systems"
// // lazy val supported?

//       // defer.verbose {
//       //   var i = await(ZIO.succeed(10))
//       //   while (await(ZIO.succeed(i - 2)) >= 0) {
//       //     println(s"Currently: $i")
//       //     await(ZIO.succeed { i = i -1 } )
//       //   }
//       // }
//       //
//       // defer.verbose {
//       //   var i = Ref.make(10).run
//       //   while (i.get.run - 2 >= 0) {
//       //     println(s"======= Currently: ${i.get.run} ===========")
//       //     i.getAndUpdate(i => i - 1).run
//       //   }
//       // }

//     val outRun =
//       zio.Unsafe.unsafe { implicit unsafe =>
//         zio.Runtime.default.unsafe.run(out).getOrThrow()
//       }
//     println("====== RESULT: " + outRun)
//   }

  // def funH() = { //
  //     val out =
  //     defer.verbose {
  //       val x = 123
  //       (await(ZIO.succeed(x)), {
  //         val a = await(ZIO.succeed(888))
  //         a
  //       }, await(ZIO.succeed(456)))
  //     }

  //     val outRun =
  //       zio.Unsafe.unsafe { implicit unsafe =>
  //         zio.Runtime.default.unsafe.run(out).getOrThrow()
  //       }
  //     println("====== RESULT: " + outRun)
  // }

  // TODO try without catch should not be allowed? what about finally?
  // TODO test this, as well as the failure case of this
  def funI() = { //
    val out =
      defer.verbose {
        try {
          // unsafe {
          // val x = 123
          // throw new RuntimeException("foo")
          // 456

          // TODO add test for this
          // val x = 123
          // (await(ZIO.succeed(x)), {
          //   val a = await(ZIO.succeed(888))
          //   a
          // }, await(ZIO.succeed(456)))

          // TODO add test for this (should throw an exception that is not caught)
          // val x = 123
          // (await(ZIO.succeed(x)), {
          //   val a = await(ZIO.succeed(888))
          //   4/0
          // }, await(ZIO.succeed(456)))

          // TODO add test for this (should throw an exception that IS caught)
          val x = 123
          unsafe {
            (
              await(ZIO.succeed(x)), {
                val a = await(ZIO.succeed(888))
                4 / 0
              },
              await(ZIO.succeed(456))
            )
          }

          // TODO example like this with single await to test the other Parallel case

          // Note, before going further need to touch-up Format again to make sure output trees are palletable
          // (maybe in future add an option to not hide tree complexity)

          // TODO Add a test for this
          // val a = await(ZIO.succeed(888))
          // await(ZIO.attempt(1/0))
        } catch {
          case _ => (999, 999, 999)
        }
      }

    // val out2 =
    //   ZIO
    //     .succeed[Int](888)
    //     //.asInstanceOf[ZIO[_ >: Nothing <: Any, _ >: Nothing <: Any, Int]]
    //     .flatMap(((v: Int) => {
    //       val a: Int = v
    //       ZIO.attempt[Int](1./(0))
    //     }))
    //     .catchSome[Nothing & Any, Any, Any](
    //       (
    //           (tryLamParam: Any) =>
    //             tryLamParam match {
    //               case _ =>
    //                 ZIO.succeed[Int](999)
    //             }
    //       ).asInstanceOf[PartialFunction[Any, ZIO[Any, Throwable, Int]]]
    //     )

    val outRun =
      zio.Unsafe.unsafe { implicit unsafe =>
        zio.Runtime.default.unsafe.run(out).getOrThrow()
      }
    println("====== RESULT: " + outRun)
  }

  def main(args: Array[String]): Unit = {
    funI()
  }

  // def printPartialFuncExample(): Unit = {
  //   println("========================Partial Function Lambda=============================")
  //   PrintMac(stuff {
  //     case x: IOException => 123
  //     case y: IllegalArgumentException => 456
  //   })
  // }

  // //   PrintMac(stuff2(
  // //     x => x.length()
  // //   ))
  // // }

  // def stuff(input: PartialFunction[String, Int]): Option[Int] =
  //   input.lift("foo").map(_ + 1)

  // def stuff2(input: Function[String, Int]): Int =
  //   input.apply("foo")

}
