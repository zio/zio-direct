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

}
