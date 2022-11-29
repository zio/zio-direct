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

object Example2 {

  def stuff(a: String, b: String) = a + b

  def queryWebsite(urlString: String): String = {
    val url = new URL(urlString);
    val out = new StringBuffer()
    val conn = url.openConnection().asInstanceOf[HttpURLConnection]
    val in = new BufferedReader(new InputStreamReader(conn.getInputStream()));
    var inputLine: String = "";
    while ({ inputLine = in.readLine(); inputLine } != null) {
      out.append(inputLine)
    }
    in.close();
    out.toString
  }

  def main(args: Array[String]): Unit = {
    val url = "https://www.google.com"

    def isTrue(str: String) = {
      ZIO.sleep(10.seconds) *> ZIO.succeed(str.startsWith("f"))
    }

    def isFoo(str: String) = str == "foo"

    val out0 =
      for {
        x <- Ref.make("foo")
        out <- {
          defer {
            if (isFoo(x.get.run)) x.set("aaa").run
            x.get.run
          }
        }
      } yield (out)

    val out = defer(Params(Verify.Strict)) {
      val x = Ref.make("000").run
      val y = Ref.make("foo").run

      if (isFoo(y.get.run)) x.set("aaa").run

      if (x.get.run == "aaa") "isAAA"
      else if (x.get.run == "bbb") "isBBB"
      else "isSomethingElse"
    }

  }

}
