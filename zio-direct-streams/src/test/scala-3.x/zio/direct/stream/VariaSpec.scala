package zio.direct.stream

import zio.direct.stream.each
import zio.direct.DeferRunSpec
import zio.test._
import zio.test.Assertion._
import zio._
import ZIO.{unsafe => _, _}
import zio.direct.core.util.Messages
import scala.annotation.nowarn
import zio.direct.DeferRunSpec
import zio.stream.ZStream

@nowarn
object VariaSpec extends DeferRunSpec {
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
              val env = ZStream.service[ConfigInt].each
              env
            }
            v.each.value + 1
          }
        assertZIO(out.provideLayer(ZLayer.succeed(ConfigInt(3))).runCollect)(equalTo(Chunk(4)))
      },
      test("four services") {
        val out =
          defer {
            val (x, y) = (ZStream.service[Config1].each.value, ZStream.service[Config2].each.value)
            val config = ZStream.service[Config3].each
            x + config.value + y + ZStream.service[Config4].each.value
          }
        val provided =
          out.provideEnvironment(
            ZEnvironment(Config1(1), Config2(2), Config3(3), Config4(4))
          )
        assertZIO(provided.runCollect)(equalTo(Chunk(10)))
      },
      test("services with match statement") {
        val out =
          defer {
            val configValue =
              ZStream.service[Config1].each match {
                case Config1(value) => value + ZStream.service[Config2].each.value
              }
            configValue + ZStream.service[Config3].each.value
          }
        val provided =
          out.provideEnvironment(
            ZEnvironment(Config1(1), Config2(2), Config3(4))
          )
        assertZIO(provided.runCollect)(equalTo(Chunk(7)))
      },
      test("double tuple deconstruct") {
        val out =
          defer {
            val (x, y) = (ZStream.succeed("foo").each, ZStream.succeed("bar").each)
            val (x1, y1) = (ZStream.succeed("A" + x).each, ZStream.succeed("B" + y).each)
            x + x1 + y + y1
          }
        assertZIO(out.runCollect)(equalTo(Chunk("fooAfoobarBbar")))
      },
      test("catch run inside run") {
        val msg = Messages.RunRemainingAfterTransformer
        import zio.direct.Internal.ignore
        runLiftFailMsg(msg) {
          """
          val x = ZStream(123).each
          val y =
            ignore {
              ZStream(456).each
            }
          x + y
          """
        }
      },
      test("deconstruct and import") {
        class Blah(val value: Int)
        val out =
          defer {
            val (a, a1) = each(ZStream.succeed((1, 2)))
            val blah = new Blah(3)
            import blah._
            val b = each(ZStream.succeed(value))
            a + b
          }
        assertZIO(out.runCollect)(equalTo(Chunk(4)))
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
            val (a, a1) = ZStream.succeed((value0, value1)).each
            val blah = new Blah(3)
            import blah._
            val b = ZStream.succeed(value).each
            a + a1 + b
          }
        assertZIO(out.runCollect)(equalTo(Chunk(6)))
      }
    )
  )
}
