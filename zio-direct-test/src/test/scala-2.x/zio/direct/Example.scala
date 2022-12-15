package zio.direct

import zio.direct.Internal.ignore
import zio.direct.examples.RunNow
import zio.direct.core.util.debug.PrintMac
import zio.ZIO._
import zio.ZIO
import java.sql.SQLException
import java.io.IOException

object Example {

  sealed trait Error[+T]
  object Error {
    case class ErrorOne[T](t: T) extends Error[T]
    case class ErrorTwo[T](t: T) extends Error[T]
  }

  def main(args: Array[String]): Unit = {

    val out =
      defer(Use.withLenientCheck) {
        if (true)
          ZIO.fail(Error.ErrorOne(new IOException("foo")))
        else
          ZIO.fail(Error.ErrorTwo(new SQLException("foo")))
      }

    println("===== Output: " + RunNow(out))
  }
}
