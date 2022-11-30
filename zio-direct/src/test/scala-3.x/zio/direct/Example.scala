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
import zio.stream.ZStream
import zio.direct.examples.RunNow

object Example {

  // TODO zio-direct test with scope

  // def funH(): Unit = {

  case class ConfigString(value: String)

  //   // Correct issue but error is misleading
  val out = (defer {
    if (
      run({
        for {
          env <- ZIO.service[ConfigString]
          value <- ZIO.succeed(env.value)
        } yield (value)
      }) == "blah"
    )
      "foo"
    else
      "barr"
  })

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

}
