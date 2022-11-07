// package zio.asyncawait

// import zio._
// import zio.asyncawait.core.util.debug.PrintMac
// import java.sql.SQLException
// import java.io.IOException

// object Example {
//   // def funA():Unit = {
//   //   // Works?
//   //   // PrintMac( lift {
//   //   //   val a = unlift(ZIO.succeed(123).asInstanceOf[Task[Int]])
//   //   //   unlift(ZIO.succeed(2).asInstanceOf[Task[Any]])
//   //   // })

//   //   // PrintMac
//   //   // val out =
//   //   // (lift {
//   //   //   val a = unlift(ZIO.succeed(123).asInstanceOf[Task[Int]])
//   //   //   unlift(ZIO.succeed(2).asInstanceOf[Task[Int]])
//   //   //   a
//   //   // })


//   //   class Blah(input: Int) {
//   //     def value: Int = input
//   //   }
//   //   // PrintMac(async {
//   //   //   val (a, a1) = await(ZIO.succeed((123, 456)))
//   //   //   val blah = new Blah(2)
//   //   //   import blah._
//   //   //   val b = await(ZIO.succeed(value))
//   //   //   a + b
//   //   // })

//   //   val out =
//   //     async {
//   //       val (a, a1) = await(ZIO.succeed((123, 456)))
//   //       val blah = new Blah(2)
//   //       import blah._
//   //       val b = await(ZIO.succeed(value))
//   //       a + b
//   //     }

//   //   val outRun =
//   //     zio.Unsafe.unsafe { implicit unsafe =>
//   //       zio.Runtime.default.unsafe.run(out).getOrThrow()
//   //     }
//   //   println("====== RESULT: " + outRun)
//   // }

//   // def funB():Unit = {

//   //   class Blah(input: Int):
//   //     def value: Int = input
//   //   class Blah0(input: Int):
//   //     def value0: Int = input
//   //   class Blah1(input: Int):
//   //     def value1: Int = input

//   //   val out =
//   //     async {
//   //       val blah0 = new Blah0(123)
//   //       import blah0._
//   //       val blah1 = new Blah1(456)
//   //       import blah1._
//   //       val (a, a1) = await(ZIO.succeed((blah0.value0, blah1.value1)))
//   //       val blah = new Blah(2)
//   //       import blah._
//   //       val b = await(ZIO.succeed(value))
//   //       a + a1 + b
//   //     }

//   //   val outRun =
//   //     zio.Unsafe.unsafe { implicit unsafe =>
//   //       zio.Runtime.default.unsafe.run(out).getOrThrow()
//   //     }
//   //   println("====== RESULT: " + outRun)
//   // }

//   // def funC(): Unit = {
//   //   val out =
//   //     PrintMac.passthrough(async {
//   //       val (a, b) = (await(async(1)), await(async(2)))
//   //       a + b
//   //     })

//   //   val outRun =
//   //     zio.Unsafe.unsafe { implicit unsafe =>
//   //       zio.Runtime.default.unsafe.run(out).getOrThrow()
//   //     }
//   //   println("====== RESULT: " + outRun)
//   // }

//   // def funD(): Unit = {
//   //   val i = async(1)
//   //   val j = async(2)

//   //   PrintMac(async {
//   //       val v = await(i)
//   //       v + await(j)
//   //     })

//   //   val out =
//   //     async {
//   //       val v = await(i)
//   //       v + await(j)
//   //     }



//   //   val outRun =
//   //     zio.Unsafe.unsafe { implicit unsafe =>
//   //       zio.Runtime.default.unsafe.run(out).getOrThrow()
//   //     }
//   //   println("====== RESULT: " + outRun)
//   // }

//   // def funE(): Unit = {
//   //   val out =
//   //     async {
//   //       def a(i: Int, s: String) = i + s.toInt
//   //       await(async(a(1, "2"))) + a(0, "1")
//   //     }

//   //   val outRun =
//   //     zio.Unsafe.unsafe { implicit unsafe =>
//   //       zio.Runtime.default.unsafe.run(out).getOrThrow()
//   //     }
//   //   println("====== RESULT: " + outRun)
//   // }

//   // def funF(): Unit = {
//   //   val out = (async {
//   //     def a = async("foo")
//   //     await(a) + "bar"
//   //   })

//   //   val outRun =
//   //     zio.Unsafe.unsafe { implicit unsafe =>
//   //       zio.Runtime.default.unsafe.run(out).getOrThrow()
//   //     }
//   //   println("====== RESULT: " + outRun)
//   // }

//   // def funG(): Unit = {
//   //   // PrintMac(async {
//   //   //   def blah(b: String) = await(async(b))
//   //   //   blah("foo") + "bar"
//   //   // })

//   //   val out = (async {
//   //     def blah(b: String)(c: String) = await(async(b + c))
//   //     blah("foo")("bar") + "baz"
//   //   })

//   //   val outRun =
//   //     zio.Unsafe.unsafe { implicit unsafe =>
//   //       zio.Runtime.default.unsafe.run(out).getOrThrow()
//   //     }
//   //   println("====== RESULT: " + outRun)
//   // }
//   //
//   //

//    // // // // // // // ..

//   def funH(): Unit = {

//     // Correct issue but error is misleading
//     // val out = (async {
//     //   if (await({
//     //     for {
//     //       env <- ZIO.service[Config]
//     //       value <- ZIO.succeed(env.value)
//     //     } yield (value)
//     //   }) == "blah")
//     //     "foo"
//     //   else
//     //     "barr"
//     // })

//     val out =
//       async.info {
//         val (x, y) = (await(ZIO.succeed("foo")), await(ZIO.succeed("bar")))
//         val (x1, y1) = (await(ZIO.succeed("foo2" + x)), await(ZIO.succeed("bar2" + y)))
//         x + x1 + y + y1
//       }

//     // Make a test for this
//     // val out =
//     //   async.verbose {
//     //     val (x, y) = (await(ZIO.succeed("foo")), await(ZIO.succeed("bar")))
//     //     val config = await(ZIO.service[Config])
//     //     x + config.value + y
//     //   }

//     // Noisy exception
//     // val out =
//     //   async.info {
//     //     val tup = (await(ZIO.succeed("foo")), await(ZIO.succeed("bar")))
//     //     val configValue =
//     //       await(ZIO.service[Config]) match {
//     //         case Config(value) => value
//     //       }
//     //     tup._1 + config.value + tup._2
//     //   }

//     // val out =
//     //   async.info {
//     //     val tup = (await(ZIO.succeed("foo")), await(ZIO.succeed("bar")))
//     //     val configValue =
//     //       await(ZIO.service[Config]) match {
//     //         case Config(value) => await(ZIO.succeed(value))
//     //       }
//     //     tup._1 + configValue + tup._2
//     //   }

//     val provided = out.provide(ZLayer.succeed(Config("x")))

//     val outRun =
//       zio.Unsafe.unsafe { implicit unsafe =>
//         zio.Runtime.default.unsafe.run(provided).getOrThrow()
//       }
//     println("====== RESULT: " + outRun)
//   }

//   def main(args: Array[String]): Unit = {
//     funH() // //
//   }
// }
