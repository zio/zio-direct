package zio.direct.pure

import zio.direct.DeferRunSpec
import zio.test._
import zio.test.Assertion._
import zio._
import ZIO.{unsafe => _, _}
import zio.direct.core.util.Messages
import scala.annotation.nowarn
import zio.direct.DeferRunSpec
import zio.prelude.fx.ZPure

@nowarn
object VariaSpec extends DeferRunSpec {
  val dc = deferWith[String, MyState]
  import dc._
  val init = MyState("init")

  case class Config1(value: Int)
  case class Config2(value: Int)
  case class Config3(value: Int)
  case class Config4(value: Int)

  class SomeService private (var isOpen: Boolean) {
    def close(): Unit = { isOpen = false }
  }
  object SomeService {
    def open() = new SomeService(true)
  }

  val spec = suite("VariaSpec")(
    suite("odd placements of defer/run")(
      test("defer in defer") {
        val out =
          defer {
            val v = defer {
              val env = Wrap.service[ConfigInt].eval
              env
            }
            v.eval.value + 1
          }
        assert(out.provideState(init).provideService(ConfigInt(3)).run)(equalTo(4))
      },
      test("four services") {
        val out =
          defer {
            val (x, y) = (Wrap.service[Config1].eval.value, Wrap.service[Config2].eval.value)
            val config = Wrap.service[Config3].eval
            x + config.value + y + Wrap.service[Config4].eval.value
          }
        val provided =
          out.provideEnvironment(
            ZEnvironment(Config1(1), Config2(2), Config3(3), Config4(4))
          )

        assert(provided.provideState(init).run)(equalTo(10))
      },
      test("services with match statement") {
        val out =
          defer {
            val configValue =
              Wrap.service[Config1].eval match {
                case Config1(value) => value + Wrap.service[Config2].eval.value
              }
            configValue + Wrap.service[Config3].eval.value
          }
        val provided =
          out.provideEnvironment(
            ZEnvironment(Config1(1), Config2(2), Config3(4))
          )
        assert(provided.provideState(init).run)(equalTo(7))
      },
      test("double tuple deconstruct") {
        val out =
          defer {
            val (x, y) = (Wrap.succeed("foo").eval, Wrap.succeed("bar").eval)
            val (x1, y1) = (Wrap.succeed("A" + x).eval, Wrap.succeed("B" + y).eval)
            x + x1 + y + y1
          }
        assert(out.provideState(init).run)(equalTo("fooAfoobarBbar"))
      },
      test("deconstruct and import") {
        class Blah(val value: Int)
        val out =
          defer {
            val (a, a1) = eval(Wrap.succeed((1, 2)))
            val blah = new Blah(3)
            import blah._
            val b = eval(Wrap.succeed(value))
            a + b
          }
        assert(out.provideState(init).run)(equalTo(4))
      },
      test("deconstruct and multiple import") {
        class Blah(val value: Int)
        class Blah0(val value0: Int)
        class Blah1(val value1: Int)
        val out =
          defer {
            val blah0 = new Blah0(1)
            import blah0._
            val blah1 = new Blah1(2)
            import blah1._
            val (a, a1) = Wrap.succeed((value0, value1)).eval
            val blah = new Blah(3)
            import blah._
            val b = Wrap.succeed(value).eval
            a + a1 + b
          }
        assert(out.provideState(init).run)(equalTo(6))
      }
    )
  )
}
