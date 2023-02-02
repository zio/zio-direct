package zio.direct.exploration

import zio.direct.core.util.debug.PrintMac

import zio.direct.exploration.{Foo, valueAbleFooReal, UseValueAble}
object UseTypeclassExample {
  def main(args: Array[String]): Unit = {
    inline def tree = UseValueAble(Foo("blahblah"))
    PrintMac(tree)
    println(tree)
  }
}
