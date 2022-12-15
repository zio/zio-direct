package zio.direct

import zio.test._
import zio.test.Assertion._
import zio._
import zio.ZIO.{unsafe => _}

object ForeachSpec extends DeferRunSpec {

  val spec = suite("ForeachSpec")(
    suite("Purity Combos")(
      test("List Pure, Body Pure") {
        var v = 1
        val out =
          defer.use(Use.withLenientCheck) {
            for (i <- List(1, 2, 3)) {
              v += i
            }
          }
        assertZIO(out)(equalTo(())) andAssert
          assert(v)(equalTo(7))
      },
      test("List Impure, body Pure") {
        var v = 1
        val out =
          defer.use(Use.withLenientCheck) {
            for (i <- ZIO.succeed(List(1, 2, 3)).run) {
              v += i
            }
          }
        assertZIO(out)(equalTo(())) andAssert
          assert(v)(equalTo(7))
      },
      test("List Pure, body Impure") {
        var v = 1
        val out =
          defer.use(Use.withLenientCheck) {
            for (i <- List(1, 2, 3)) {
              ZIO.succeed(v += i).run
            }
          }
        assertZIO(out)(equalTo(())) andAssert
          assert(v)(equalTo(7))
      },
      test("List Impure, body Impure") {
        var v = 1
        val out =
          defer.use(Use.withLenientCheck) {
            for (i <- ZIO.succeed(List(1, 2, 3)).run) {
              ZIO.succeed(v += i).run
            }
          }
        assertZIO(out)(equalTo(())) andAssert
          assert(v)(equalTo(7))
      }
    ),
    suite("Dependency tests")(
      test("Dependency in list") {
        var v = 1
        val out =
          defer.use(Use.withLenientCheck) {
            for (i <- ZIO.service[ConfigT[List[Int]]].run.value) {
              v += i
            }
          }
        val provided =
          out.provide(
            ZLayer.succeed(ConfigT(List(1, 2, 3)))
          )
        assertZIO(provided)(equalTo(())) andAssert
          assert(v)(equalTo(7))
      },
      test("Dependency in body") {
        var v = 1
        val out =
          defer.use(Use.withLenientCheck) {
            for (i <- List(1, 2, 3)) {
              val assign = ZIO.service[ConfigInt].run.value
              v += assign + i
            }
          }

        val provided =
          out.provide(
            ZLayer.succeed(ConfigInt(1))
          )
        assertZIO(provided)(equalTo(())) andAssert
          assert(v)(equalTo(10))
      },
      test("Dependency in both") {
        var v = 1
        val out =
          defer.use(Use.withLenientCheck) {
            for (i <- ZIO.service[ConfigT[List[Int]]].run.value) {
              val assign = ZIO.service[ConfigInt].run.value
              v += assign + i
            }
          }

        val provided =
          out.provide(
            ZLayer.succeed(ConfigT(List(1, 2, 3))),
            ZLayer.succeed(ConfigInt(1))
          )
        assertZIO(provided)(equalTo(())) andAssert
          assert(v)(equalTo(10))
      }
    ),
    suite("Error tests")(
      test("Throw in List die") {
        var v = 1
        val out =
          defer.use(Use.withLenientCheck) {
            for (i <- ZIO.succeed(List(1, { throwFoo(); 2 }, 3)).run) {
              ZIO.succeed(v += i).run
            }
          }
        assertZIO(out.exit)(dies(isSubtype[FooError](anything))) andAssert
          assert(v)(equalTo(1))
      },
      test("Throw in List fail") {
        var v = 1
        val out =
          defer.use(Use.withLenientCheck) {
            for (i <- unsafe(ZIO.succeed(List(1, { throwFoo(); 2 }, 3)).run)) {
              ZIO.succeed(v += i).run
            }
          }
        assertZIO(out.exit)(dies(isSubtype[FooError](anything))) andAssert
          assert(v)(equalTo(1))
      }
    )
  )
}
