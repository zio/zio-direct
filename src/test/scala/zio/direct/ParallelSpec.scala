package zio.direct

import zio.direct.{run => runBlock} // //
import zio.test._
import zio.test.Assertion._
import zio.direct.core.util.debug.PrintMac
import zio._
import ZIO._
import zio.direct.Dsl.Params
import zio.direct.core.metaprog.Verify
import zio.direct.core.util.TraceType
import zio.direct.core.metaprog.TypeUnion

object ParallelSpec extends AsyncAwaitSpec {

  val spec = suite("ParallelSpec")(
    suite("Multi Await Tests") {
      test("Inferred Environment Should be Correct") {
        val out = defer {
          (service[ConfigInt].run.value, service[ConfigString].run.value)
        }
        // TODO specifc test for the type
        val provided = out.provide(ZLayer.succeed(ConfigInt(1)), ZLayer.succeed(ConfigString("two")))
        assertZIO(provided)(Assertion.equalTo((1, "two")))
      }
      +
      test("Inferred Environment Should be Correct - single await in parallels") {
        val out = defer {
          (111, service[ConfigString].run.value, 222)
        }
        // TODO specifc test for the type
        val provided = out.provide(ZLayer.succeed(ConfigString("two")))
        assertZIO(provided)(Assertion.equalTo((111, "two", 222)))
      }
      +
      test("Inferred Environment one side error") {
        val out = defer {
          (
            service[ConfigInt].run.value,
            runBlock(service[ConfigString].flatMap(v => fail(v.value)))
          )
        }
        val provided = out.provide(ZLayer.succeed(ConfigInt(1)), ZLayer.succeed(ConfigString("two")))
        assertZIO(provided.exit)(fails(equalTo("two")))
      }
      +
      test("Inferred Environment both sides error") {
        val out = defer {
          (
            runBlock(service[ConfigInt].flatMap(v => fail(v.value))),
            runBlock(service[ConfigString].flatMap(v => fail(v.value)))
          )
        }
        val provided = out.provide(ZLayer.succeed(ConfigInt(1)), ZLayer.succeed(ConfigString("two")))
        assertZIO(provided.exit)(fails(anything))
      }
      +
      test("Inferred Environment both sides error impure") {
        val out = defer {
          (fail(makeFooError).run, fail(makeBarError).run)
        }
        assertZIO(out.exit)(fails(anything))
      }
      +
      test("Inferred Environment both sides error impure - least base type") {
        val out = defer.info(Params(TypeUnion.LeastUpper)) {
          (fail(makeFooError).run, fail(makeBarError).run)
        }
        assertZIO(out.exit)(fails(anything))
      }
    }
  )
}
