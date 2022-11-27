package zio.direct.examples

import zio.ZIO
import zio.direct._

object BreakingExamples {

  object Model {
    def httpGet(url: String): ZIO[Any, Throwable, String] = ???
  }
  import Model._



}
