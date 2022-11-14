// package zio.direct

// import zio.test._
// import zio.direct.core.util.debug.PrintMac

// object BlockSpec extends AsyncAwaitSpec {
//   def spec =
//     suite("BlockSpec") (
//       suite("assigned await") {
//         test("only") {
//           val i = defer(1)
//           runLiftTest(1) {
//             val v = run(i)
//             v
//           }
//         }
//         +
//         test("followed by pure expression") {
//           val i = defer(1)
//           runLiftTest(2) {
//             val v = run(i)
//             v + 1
//           }
//         }
//         +
//         test("followed by impure expression") {
//           val i = defer(1)
//           val j = defer(2)
//           runLiftTest(3) {
//             val v = run(i)
//             v + run(j)
//           }
//         }
//         +
//         test("nested") {
//           val i = defer(1)
//           runLiftTest(3) {
//             val v = {
//               val r = run(i)
//               r + 1
//             }
//             v + 1
//           }
//         }
//       },
//       suite("unassigned await") {
//         test("only") {
//           val i = defer(1)
//           runLiftTest(1) {
//             run(i)
//           }
//         }
//         +
//         test("followed by pure expression") {
//           val i = defer(1)
//           runLiftTest(2) {
//             run(i)
//             2
//           }
//         }
//         +
//         test("followed by impure expression") {
//           val i = defer(1)
//           val j = defer(2)
//           runLiftTest(2) {
//             run(i)
//             run(j)
//           }
//         }
//       },
//       suite("pure expression") {
//         test("only") {
//           runLiftTest(1) {
//             1
//           }
//         }
//         +
//         test("followed by pure expression") {
//           def a = 1
//           runLiftTest(2) {
//             a
//             2
//           }
//         }
//         +
//         test("followed by impure expression") {
//           val i = defer(1)
//           def a = 2
//           runLiftTest(1) {
//             a
//             run(i)
//           }
//         }
//       },
//       suite("complex") {
//         // test is not working
//         test("tuple val pattern") {
//           runLiftTest(3) {
//             val (a, b) = (run(defer(1)), run(defer(2)))
//             a + b
//           }
//         }
//         +
//         test("assignment not allowed") {
//           val i = defer(1)
//           runLiftFail {
//             """
//             var x = 0
//             x += {
//               val v = run(i) + 2
//               v + 1
//             }
//             x
//             """
//           }
//         }
//       }
//     )
// }
