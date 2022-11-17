package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio.direct.core.util.debug.PrintMac
import zio._
import ZIO._
import zio.direct.Dsl.Params
import zio.direct.core.metaprog.Verify

object ParallelSpec extends AsyncAwaitSpec {

  val spec = suite("ParallelSpec")(
    suite("Multi Await Tests") {
      test("Inferred Environment Should be Correct") {
        val out = defer.verbose {
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
        assertZIO(provided.exit)(Assertion.fails(Assertion.equalTo("two")))
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
        assertZIO(provided.exit)(Assertion.fails(Assertion.equalTo(1)))
      }
    }
  )
}
