package zio.direct.examples

import zio._
import zio.direct._

object Refs {

  // Easy to get wrong! This keeps running forever
  // object IterationUsingFunctional {
  //   val i: UIO[Ref[Int]] = Ref.make(10)
  //   def whileLoop(): ZIO[Any, Nothing, Unit] =
  //     i.flatMap { i0 =>
  //       i0.get.flatMap { iv =>
  //         if (iv > 0) {
  //           println(s"Currently: ${iv}")
  //           i0.update(i => i - 1).flatMap { _ =>
  //             whileLoop()
  //           }
  //         } else {
  //           ZIO.succeed(())
  //         }
  //       }
  //     }
  // }

  object IterationUsingFunctional {
    val out = {
      val i: UIO[Ref[Int]] = Ref.make(10)
      i.flatMap { i0 =>
        def whileLoop(): ZIO[Any, Nothing, Unit] =
          i0.get.flatMap { iv =>
            if (iv > 0) {
              println(s"Currently: ${iv}")
              i0.update(i => i - 1).flatMap { _ =>
                whileLoop()
              }
            } else {
              ZIO.unit
            }
          }
        whileLoop()
      }
    }
  }

  object IterationUsingDefer {
    val out =
      defer {
        val i: Ref[Int] = Ref.make(10).run
        while (i.get.run > 0) {
          println(s"Currently: ${i.get.run}")
          i.update(i => i - 1).run
        }
      }
  }

  object IterationUsingWhile {
    val out = {
      var i: Int = 10
      while (i > 0) {
        println(s"Currently: ${i}")
        i = i - 1
      }
    }
  }

  def main(args: Array[String]): Unit = {
    RunNow(IterationUsingFunctional.out)
  }
}
