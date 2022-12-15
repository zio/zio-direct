package zio.direct

import zio.direct.Internal.ignore
import zio.direct.core.util.debug.PrintMac
import zio.ZIO._
import zio.ZIO
import java.sql.SQLException
import java.io.IOException
import zio.Ref

object Example {

  val out =
    defer {
      val i = Ref.make(0).run
      while (i.get.run < 3)
        i.getAndUpdate(i => i + 1).run
      i.get.run
    }

  // println("===== Output: " + RunNow(out))
  println(out)

}
