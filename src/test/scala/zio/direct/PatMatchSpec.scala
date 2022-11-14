// package zio.direct

// import zio.test._
// import zio.direct.core.util.debug.PrintMac
// import zio._

// object PatMatchSpec extends AsyncAwaitSpec {

//   val spec = suite("PatMatchSpec")(
//     suite("unlifted scrutinee") (
//       suite("without guards") {
//         test("pure cases") {
//           runLiftTest(3) {
//             run(defer("b")) match {
//               case "a" => 2
//               case "b" => 3
//             }
//           }
//         }
//         +
//         test("pure cases with val") {
//           runLiftTest(3) {
//             val v = run(defer("b"))
//             v match {
//               case "a" => 2
//               case "b" => 3
//             }
//           }
//         }
//         +
//         test("pure/impure cases") {
//           runLiftTest(2) {
//             run(defer("a")) match {
//               case "a" => run(defer(2))
//               case "b" => 3
//             }
//           }
//         }
//         +
//         test("impure cases") {
//           runLiftTest(3) {
//             run(defer("b")) match {
//               case "a" => run(defer(2))
//               case "b" => run(defer(3))
//             }
//           }
//         }
//       },
//       suite("with guards") {
//         test("pure cases") {
//           runLiftTest(3) {
//             run(defer("b")) match {
//               case s if s == "a" => 2
//               case "b"           => 3
//             }
//           }
//         }
//         +
//         test("pure/impure cases") {
//           runLiftTest(2) {
//             run(defer("a")) match {
//               case "a"           => run(defer(2))
//               case s if s == "b" => 3
//             }
//           }
//         }
//         +
//         test("impure cases") {
//           runLiftTest(2) {
//             run(defer("b")) match {
//               case s if "1".toInt == 1 => run(defer(2))
//               case "b"                 => run(defer(3))
//             }
//           }
//         }
//       }
//     ),
//     suite("pure scrutinee") (
//       suite("without guards") {
//         test("pure cases") {
//           runLiftTest(3) {
//             "b" match {
//               case "a" => 2
//               case "b" => 3
//             }
//           }
//         }
//         +
//         test("pure/impure cases") {
//           runLiftTest(2) {
//             run(defer("a")) match {
//               case "a" => run(defer(2))
//               case "b" => 3
//             }
//           }
//         }
//         +
//         test("impure cases") {
//           runLiftTest(3) {
//             "b" match {
//               case "a" => run(defer(2))
//               case "b" => run(defer(3))
//             }
//           }
//         }
//       },
//       suite("with guards") {
//         test("pure cases") {
//           runLiftTest(3) {
//             "b" match {
//               case s if s == "a" => 2
//               case "b"           => 3
//             }
//           }
//         }
//         +
//         test("pure/impure cases") {
//           runLiftTest(2) {
//             run(defer("a")) match {
//               case "a"           => run(defer(2))
//               case s if s == "b" => 3
//             }
//           }
//         }
//         +
//         test("impure cases") {
//           runLiftTest(2) {
//             "b" match {
//               case s if "1".toInt == 1 => run(defer(2))
//               case "b"                 => run(defer(3))
//             }
//           }
//         }
//       }
//     ),
//     suite("misc") {
//       test("val patmatch") {
//         runLiftTest(1) {
//           val Some(a) = run(defer(Some(1)))
//           a
//         }

//       // "invalid unlifted guard" in pendingUntilFixed {
//       //   runLiftTest(2) {
//       //     "b" match {
//       //       case s if run(defer(true)) => 2
//       //       case "b"                     => run(defer(3))
//       //     }
//       //   }
//       }
//     }
//   )
// }
