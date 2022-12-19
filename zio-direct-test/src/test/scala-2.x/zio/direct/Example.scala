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

  def fun(foo: String = "foo", bar: String = "bar") = foo + bar

  def main(args: Array[String]): Unit = {

    var x = 123

    PrintMac.detail(fun(bar = "blah")) // // //
  }
}
