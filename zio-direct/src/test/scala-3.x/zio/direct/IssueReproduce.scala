package zio.direct

import zio._
import zio.direct.core.util.debug.PrintMac
import zio.direct.{run => runBlock}
import zio.stream.ZStream
import java.io.FileInputStream
import scala.io.Source

class MyAnnot extends scala.annotation.StaticAnnotation

object MyExample {
  class FooError extends Exception("foo")
  def throwFoo() = throw new FooError
  def makeFooError = new FooError

  def main(args: Array[String]): Unit = {

    // Recompile slowness is ultimately comping from NonEmptyChunk in functions like fromPrimaryWithOthers
    val op =
      defer {

        val x = ZIO.succeed(348)
        val y = ZIO.succeed("saasdjfgsdfdfsdfgsdfsdfdfsdfsdddndfdfdf")
        val xx = x.run
        xx + (
          if xx > 0 then y.run.length() * x.run
          else y.run.length()
        )
      }

    val v = 123

    zio.Unsafe.unsafe { implicit unsafe =>
      zio.Runtime.default.unsafe.run(op).getOrThrow()
    }
  }

}
