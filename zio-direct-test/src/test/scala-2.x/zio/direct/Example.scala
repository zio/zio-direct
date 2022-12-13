// package zio.direct

// import zio.direct.Internal.ignore
// import zio.direct.examples.RunNow
// import zio.direct.core.util.debug.PrintMac
// import zio.ZIO._
// import zio.ZIO

// object Example {

//   // TODO Need a test in the Scala 2 space with a couple constructs
//   // that specifically does NOT import zio to make sure we don't have a missing (zio.)ZIO
//   // anywhere in the Scala 2 macros.
//   def main(args: Array[String]): Unit = { // // // // // // // //

//     var v = 1
//     val out =
//       defer.verbose(Use.withLenientCheck) {
//         for (i <- ZIO.succeed(List(1, 2, 3)).run) {
//           v += i
//         }
//       }

//     println("===== Output: " + RunNow(out))
//   }
// }
