package zio.direct

import zio._
import zio.ZIO._
import zio.direct.core.util.debug.PrintMac
import java.sql.SQLException
import java.io.IOException
import java.net.HttpURLConnection
import java.net.URL
import java.io.BufferedReader
import java.io.InputStreamReader
import zio.direct.core.metaprog.Collect
import zio.direct.core.metaprog.Verify
import zio.direct.Dsl.Params
import scala.collection.mutable.ArrayBuffer
import zio.direct._

object BoomExamples {

  class Boom extends Exception("Boom!")

  // def main(args: Array[String]): Unit = {
  //   runNow(ZIO.attempt(throw new Boom()))
  // }

// def `"pureCode"`() = throw new Boom()
// defer { `"pureCode"`() }

}
