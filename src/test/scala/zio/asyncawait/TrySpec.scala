// package zio.asyncawait

// import zio.test._
// import zio.asyncawait.core.util.debug.PrintMac

// object TrySpec extends AsyncAwaitSpec {

//   val e = new Exception()

//   def spec =
//     suite("failure") (
//       suite("try pure") {
//         test("catch pure") {
//           runLiftTest(2) {
//             try {
//               throw e
//             } catch {
//               case `e` => 2
//             }
//           }
//         }
//         +
//         test("catch impure ======") {
//           runLiftTest(2) {
//             try {
//               throw e
//             } catch {
//               case `e`         => await(async(2))
//             }
//           }
//         }
//         +
//         test("catch pure/impure") {
//           runLiftTest(1) {
//             try {
//               throw e
//             } catch {
//               case `e`          => 1
//               case _: Throwable => await(async(2))
//             }
//           }
//         }
//       }
//     )

//   // val suiteSuccess =
//   //   suite("success") {
//   //     suite("try pure") {
//   //       test("catch pure") {
//   //         runLiftTest(1) {
//   //           try 1
//   //           catch {
//   //             case `e` => 2
//   //           }
//   //         }
//   //       }
//   //       test("catch impure") {
//   //         runLiftTest(1) {
//   //           try 1
//   //           catch {
//   //             case `e` => await(async(2))
//   //           }
//   //         }
//   //       }
//   //       test("catch pure/impure") {
//   //         runLiftTest(1) {
//   //           try 1
//   //           catch {
//   //             case `e`          => 2
//   //             case _: Throwable => await(async(3))
//   //           }
//   //         }
//   //       }
//   //     }
//   //   }

//   // val suiteFinally = suite("finally") {
//   //   test("pure") {
//   //     runLiftTest(true) {
//   //       var called = false
//   //       def c(): Unit = called = true
//   //       val _ =
//   //         try await(async(1))
//   //         finally {
//   //           c()
//   //         }
//   //       called
//   //     }
//   //   }
//   //   test("without catch") {
//   //     runLiftTest(true) {
//   //       var called = false
//   //       def c() = called = await(async(true))
//   //       try
//   //         await(async(1))
//   //       finally {
//   //         c()
//   //       }
//   //       called
//   //     }
//   //   }
//   //   test("as the only impure") {
//   //     runLiftTest(true) {
//   //       var called = false
//   //       def c() = called = await(async(true))
//   //       val _ =
//   //         try 1
//   //         catch {
//   //           case `e`          => 2
//   //           case _: Throwable => 3
//   //         } finally {
//   //           c()
//   //         }
//   //       called
//   //     }
//   //   }
//   // }
// }