package zio.direct

import zio._
import zio.direct.core.util.debug.PrintMac
import java.sql.SQLException
import java.io.IOException
import zio.direct.core.metaprog.Collect
import zio.direct.core.metaprog.Verify
import javax.sql.DataSource
import zio.direct.Dsl.Params
import java.sql.Connection
import zio.Exit.{Failure, Success}

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
  //   //   val (a, a1) = run(ZIO.succeed((123, 456)))
  //   //   val blah = new Blah(2)
  //   //   import blah._
  //   //   val b = run(ZIO.succeed(value))
  //   //   a + b
  //   // })

  //   val out =
  //     defer {
  //       val (a, a1) = run(ZIO.succeed((123, 456)))
  //       val blah = new Blah(2)
  //       import blah._
  //       val b = run(ZIO.succeed(value))
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
  //       val (a, a1) = run(ZIO.succeed((blah0.value0, blah1.value1)))
  //       val blah = new Blah(2)
  //       import blah._
  //       val b = run(ZIO.succeed(value))
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
  //       val (a, b) = (run(defer(1)), run(defer(2)))
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
  //       val v = run(i)
  //       v + run(j)
  //     })

  //   val out =
  //     defer {
  //       val v = run(i)
  //       v + run(j)
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
  //       run(defer(a(1, "2"))) + a(0, "1")
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
  //     run(a) + "bar"
  //   })

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

  // def funG(): Unit = {
  //   // PrintMac(defer {
  //   //   def blah(b: String) = run(defer(b))
  //   //   blah("foo") + "bar"
  //   // })

  //   val out = (defer {
  //     def blah(b: String)(c: String) = run(defer(b + c))
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
  //   //   if (run({
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
  //   //     val (x, y) = (run(ZIO.succeed("foo")), run(ZIO.succeed("bar")))
  //   //     val (x1, y1) = (run(ZIO.succeed("foo2" + x)), run(ZIO.succeed("bar2" + y)))
  //   //     x + x1 + y + y1
  //   //   }

  //   // Make a test for this
  //   // val out =
  //   //   defer.verbose {
  //   //     val (x, y) = (run(ZIO.succeed("foo")), run(ZIO.succeed("bar")))
  //   //     val config = run(ZIO.service[Config])
  //   //     x + config.value + y
  //   //   }

  //   // Noisy exception
  //   // val out =
  //   //   defer.info {
  //   //     val tup = (run(ZIO.succeed("foo")), run(ZIO.succeed("bar")))
  //   //     val configValue =
  //   //       run(ZIO.service[Config]) match {
  //   //         case Config(value) => value
  //   //       }
  //   //     tup._1 + config.value + tup._2
  //   //   }

  //   // val out =
  //   //   defer.info {
  //   //     val tup = (run(ZIO.succeed("foo")), run(ZIO.succeed("bar")))
  //   //     val configValue =
  //   //       run(ZIO.service[Config]) match {
  //   //         case Config(value) => run(ZIO.succeed(value))
  //   //       }
  //   //     tup._1 + configValue + tup._2
  //   //   }

  //   val out =
  //     defer {
  //       def a(i: Int, s: String) = run(defer(i)) + s.toInt
  //       a(1, "2") + run(defer(1))
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
  //             //run(ZIO.attempt("foo"))
  //             run(ZIO.attempt { throw new IOException("blah") })
  //           } catch {
  //             case e: IOException => e.getMessage()
  //           } finally {
  //             println("============ Recovering ========")
  //           }
  //         )
  //       val msgResult = run(msg)

  //       run(ZIO.succeed(msgResult))
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
  //   //     val aResult = run(a.join)
  //   //     zio.Console.printLine(s"A completed: ${(currTime() - startTime)/1000d}").run
  //   //     val bResult = run(b.join)
  //   //     zio.Console.printLine(s"B completed: ${(currTime() - startTime)/1000d}").run
  //   //     (aResult, bResult)
  //   //   }

  //   // def currTime(): Double = java.lang.System.currentTimeMillis()
  //   // val out =
  //   //   defer.verbose {
  //   //     val startTime = currTime()
  //   //     val a = run(ZIO.collectAllPar(Chunk(ZIO.sleep(10.seconds), ZIO.sleep(2.seconds))))
  //   //     run(zio.Console.printLine(s"Completed in: ${(currTime() - startTime)/1000d}"))
  //   //     a
  //   //   }

  //   // val out =
  //   //   defer.info {
  //   //     (run(ZIO.sleep(5.seconds)), run(ZIO.sleep(10.seconds)))
  //   //   }

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

  // while ({ val v = run(i.get) - 2; println(v); v } > 0) { /

//   def funJ() = {

//     val out =
//       defer.verbose {
//         var i = 10
//         (run(ZIO.succeed({ i = i - 1 })), run(ZIO.succeed({ i = i - 2 })))
//       }

// // (TODO this should also give a warning)
// // emphasize "for effect systems"
// // lazy val supported?

//       // defer.verbose {
//       //   var i = run(ZIO.succeed(10))
//       //   while (run(ZIO.succeed(i - 2)) >= 0) {
//       //     println(s"Currently: $i")
//       //     run(ZIO.succeed { i = i -1 } )
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
  //       (run(ZIO.succeed(x)), {
  //         val a = run(ZIO.succeed(888))
  //         a
  //       }, run(ZIO.succeed(456)))
  //     }

  //     val outRun =
  //       zio.Unsafe.unsafe { implicit unsafe =>
  //         zio.Runtime.default.unsafe.run(out).getOrThrow()
  //       }
  //     println("====== RESULT: " + outRun)
  // }

  // TODO try without catch should not be allowed? what about finally?
  // TODO test this, as well as the failure case of this
  // def funI() = { //
  //   val out =
  //     defer.verbose {
  //       try {
  //         // unsafe {
  //         // val x = 123
  //         // throw new RuntimeException("foo")
  //         // 456

  //         // TODO add test for this
  //         // val x = 123
  //         // (run(ZIO.succeed(x)), {
  //         //   val a = run(ZIO.succeed(888)) //
  //         //   a
  //         // }, run(ZIO.succeed(456)))

  //         // TODO add test for this (should throw an exception that is not caught)
  //         // val x = 123
  //         // (run(ZIO.succeed(x)), {
  //         //   val a = run(ZIO.succeed(888))
  //         //   4/0
  //         // }, run(ZIO.succeed(456)))

  //         // TODO add test for this (should throw an exception that IS caught)
  //         // val x = 123
  //         // unsafe {
  //         //   (
  //         //     run(ZIO.succeed(x)), {
  //         //       val a = run(ZIO.succeed(888))
  //         //       4 / 0
  //         //     },
  //         //     run(ZIO.succeed(456))
  //         //   )
  //         // }

  //         val x = 123
  //         unsafe {
  //           (
  //             run(ZIO.succeed(x)), {
  //               val a = ZIO.succeed(888).run
  //               4 / 0
  //             },
  //             ZIO.succeed(456).run
  //           )
  //         }

  //         // TODO example like this with single await to test the other Parallel case

  //         // Note, before going further need to touch-up Format again to make sure output trees are palletable
  //         // (maybe in future add an option to not hide tree complexity)

  //         // TODO Add a test for this
  //         // val a = run(ZIO.succeed(888))
  //         // run(ZIO.attempt(1/0))
  //       } catch {
  //         case _ => (999, 999, 888)
  //       }
  //     }

  //   // val out2 =
  //   //   ZIO
  //   //     .succeed[Int](888)
  //   //     //.asInstanceOf[ZIO[_ >: Nothing <: Any, _ >: Nothing <: Any, Int]]
  //   //     .flatMap(((v: Int) => {
  //   //       val a: Int = v
  //   //       ZIO.attempt[Int](1./(0))
  //   //     }))
  //   //     .catchSome[Nothing & Any, Any, Any](
  //   //       (
  //   //           (tryLamParam: Any) =>
  //   //             tryLamParam match {
  //   //               case _ =>
  //   //                 ZIO.succeed[Int](999)
  //   //             }
  //   //       ).asInstanceOf[PartialFunction[Any, ZIO[Any, Throwable, Int]]]
  //   //     )

  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun) // //
  // }

  // class FooException extends Exception("foo")
  // def makeEx(): Throwable = new FooException()

  // def main(args: Array[String]): Unit = {
  //   val e = new Exception("blah") // // // //
  //   val out =
  //     defer { //
  //       try {
  //         throw e
  //       } catch {
  //         case `e` => 1
  //         // case _: Throwable => run(defer(2)) // .. ...
  //       } //
  //     }
  //   val outRun =
  //     zio.Unsafe.unsafe { implicit unsafe =>
  //       zio.Runtime.default.unsafe.run(out).getOrThrow()
  //     }
  //   println("====== RESULT: " + outRun)
  // }

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

  import ZIO._
  import zio.Console.printLine

  def main(args: Array[String]): Unit = {
    // val out: ZIO[ConfigFoo, Nothing, Unit] =
    //   defer {
    //     val x = succeed(1).run
    //     val y = defer {
    //       val i = service[ConfigFoo].run
    //       succeed(3 + i.value + x)
    //     }.run
    //     val z = succeed(y.run + 4)
    //   }

    val currentConnection: FiberRef[Option[Connection]] =
      Unsafe.unsafe { implicit unsafe =>
        Runtime.default.unsafe.run(zio.Scope.global.extend(FiberRef.make(Option.empty[java.sql.Connection]))).getOrThrow()
      }

    def stuff[R <: DataSource, A](op: ZIO[R, Throwable, A]) = {
      val out = defer.verboseTree(Params(Verify.Lenient)) {
        currentConnection.get.run match {
          case Some(conn) => op.run
          case None =>
            val newConnection = defer {
              val env = ZIO.service[DataSource].run
              val connection = scopedBestEffort(attemptBlocking(env.getConnection)).run
              // Get the current value of auto-commit
              val prevAutoCommit = attemptBlocking(connection.getAutoCommit).run
              // Disable auto-commit since we need to be able to roll back. Once everything is done, set it
              // to whatever the previous value was.
              ZIO.acquireRelease(attemptBlocking(connection.setAutoCommit(false))) { _ =>
                attemptBlocking(connection.setAutoCommit(prevAutoCommit)).orDie
              }.run
              ZIO.acquireRelease(currentConnection.set(Some(connection))) { _ =>
                // Note. We are failing the fiber if auto-commit reset fails. For some circumstances this may be too aggresive.
                // If the connection pool e.g. Hikari resets this property for a recycled connection anyway doing it here
                // might not be necessary
                currentConnection.set(None)
              }.run
              ZIO.addFinalizerExit {
                case Success(_)     => blocking(ZIO.succeed(connection.commit()))
                case Failure(cause) => blocking(ZIO.succeed(connection.rollback()))
              }.run
            }

            ZIO.scoped(defer {
              newConnection.run
              op.run
            }).run
        }
      }
    }

    def scopedBestEffort[R, E, A <: AutoCloseable](effect: ZIO[R, E, A]): ZIO[R with Scope, E, A] =
      ZIO.acquireRelease(effect)(resource =>
        ZIO.attemptBlocking(resource.close()).tapError(e => ZIO.attempt(println("foo")).ignore).ignore
      )

    // .provide(ZLayer.succeed(ConfigFoo(123)))
    // val outRun =
    //   zio.Unsafe.unsafe { implicit unsafe =>
    //     zio.Runtime.default.unsafe.run(out).getOrThrow()
    //   }
    // println("====== RESULT: " + outRun) // //
  }
}
