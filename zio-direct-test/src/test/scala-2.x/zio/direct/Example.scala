package zio.direct

import zio.direct.Internal.ignore
import zio.direct.examples.RunNow
import zio.direct.core.util.debug.PrintMac
import zio.ZIO._
import zio.ZIO
import java.sql.SQLException
import java.io.IOException

object Example {

  sealed trait Error
  object Error {
    class ErrorOne extends Error
    class ErrorTwo extends Error
  }

  def main(args: Array[String]): Unit = {
    val x =
      if (true)
        new java.io.IOException("foo")
      else
        new java.sql.SQLException("bar")

    // val out =
    //   defer(Use.withLenientCheck) {
    //     if (true)
    //       ZIO.fail(Error.ErrorOne(new IOException("foo")))
    //     else
    //       ZIO.fail(Error.ErrorTwo(new SQLException("foo")))
    //   }

    // println("===== Output: " + RunNow(out))
  }
}
