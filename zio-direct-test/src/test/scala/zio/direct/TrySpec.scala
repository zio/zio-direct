package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio.ZIO
import zio.ZIO.{unsafe => _, _}

object TrySpec extends DeferRunSpec {

  val e = new Exception("blah")
  val e1 = new Exception("blahblah")

  def spec =
    suite("TrySpec")(
      suiteSuccess,
      suiteFailure,
      suiteFinally
    ) @@ TestAspect.sequential

  val suiteSuccess =
    suiteAll("success") {
      suiteAll("try pure") {
        test("catch pure") {
          // TODO if we remove the :Int widenings it will say "Wrong type expected"
          // because 1 and 2 will be treated as type-literals but zio-direct will widen
          // it to Int. Should look into these cases and how they need to be addressed.
          runLiftTest(1: Int) {
            try 1
            catch {
              case `e` => 2: Int
            }
          }
        }
        test("catch impure") {
          runLiftTest(1) {
            try 1
            catch {
              case `e` => runBlock(defer(2))
            }
          }
        }
        test("catch pure/impure") {
          runLiftTest(1) {
            try 1
            catch {
              case `e`          => 2
              case _: Throwable => runBlock(defer(3))
            }
          }
        }
      }
    }

  val suiteFailure =
    suite("failure")(
      suite("try pure")(
        test("catch pure") {
          runLiftTest(2) {
            try {
              throw e
            } catch {
              case `e` => 2
            }
          }
        },
        test("catch impure") {
          runLiftTest(2) {
            try {
              throw e
            } catch {
              case `e` => runBlock(defer(2))
            }
          }
        },
        test("catch pure/impure") {
          val out =
            defer {
              try {
                throw e
              } catch {
                case `e`          => 1
                case _: Throwable => runBlock(defer(2))
              }
            }

          assertIsType[ZIO[Any, Exception, Int]](out) andAssert
            assertZIO(out)(equalTo(1))
        },
        test("catch impure/impure") {
          val out =
            defer {
              try {
                throw (ZIO.succeed(e).run)
              } catch {
                case `e`          => 1
                case _: Throwable => ZIO.succeed(2).run
              }
            }
          assertIsType[ZIO[Any, Exception, Int]](out) andAssert
            assertZIO(out)(equalTo(1))
        },
        test("not caught error in parallel block (die-channel) - one side") {
          // even thought we create the exception inside a success block, the actual
          // 'throw' term is being used so a ZIO.fail(error) is called. therefore
          // we have the error in the fail channel so it is not a defect.
          val out = defer {
            try {
              // Only one kind of error so the error-type should be FooError either way
              (123, { throw succeed(makeFooError).run })
            } catch {
              case `e` => (111, 222)
            }
          }

          assertZIO(out.exit)(fails(isSubtype[FooError](anything))) andAssert
            assertIsType[ZIO[Any, FooError, Tuple2[Int, Int]]](out)
        },
        test("not caught error in parallel block (die-channel) - both sides") {
          val out = defer(Use.withAbstractError) {
            try {
              ({ throw ZIO.succeed(makeFooError).run }, { throw ZIO.succeed(makeBarError).run })
            } catch {
              case `e` => (111, 222)
            }
          }
          assertIsType[ZIO[Any, Exception, Tuple2[Int, Int]]](out) andAssert
            assertZIO(out.exit)(fails(isSubtype[FooError](anything)))

        }
      )
    )

  val suiteFinally = suiteAll("finally") {
    test("pure") {
      var called = false
      def c(): Unit = called = true
      runLiftTestLenient(true) {
        val _ =
          try {
            runBlock(defer(1))
          } finally {
            c()
          }
        called
      }
    }
    test("without catch") {
      var called = false
      def c() = called = true
      runLiftTestLenient(true) {
        try runBlock(defer(1))
        finally {
          c()
        }
        called
      }
    }
    test("as the only impure") {
      var called = false
      def c() = called = true
      // runLiftTest(true)
      val out =
        defer(Use.withLenientCheck) {
          val _ =
            try 1
            catch {
              case `e`          => 2
              case _: Throwable => 3
            } finally {
              c()
            }
          called
        }

      assertZIO(out)(equalTo(true))
    }
  }
}
