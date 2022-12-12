// package zio.direct

// import zio.direct.Internal.ignore
// import zio.direct.examples.RunNow
// import zio.direct.core.util.debug.PrintMac
// import zio.ZIO._

// object Example {

//   class FooError extends Exception("foo")
//   def throwFoo() = throw new FooError
//   def makeFooError = new FooError

//   // TODO Need a test in the Scala 2 space with a couple constructs
//   // that specifically does NOT import zio to make sure we don't have a missing (zio.)ZIO
//   // anywhere in the Scala 2 macros.
//   def main(args: Array[String]): Unit = { // // // // // //

//     val out = defer.verbose {
//       val x = succeed(123).run
//       val y =
//         ignore {
//           succeed(456).run
//         }
//       x + y
//     }
//     println("===== Output: " + RunNow(out))
//   }
// }
