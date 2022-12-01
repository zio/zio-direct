package zio.direct

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio.direct.core.util.debug.PrintMac
import zio._
import ZIO.{unsafe => _, _}
import zio.direct.core.metaprog.Verify
import zio.direct.Dsl.Params
import zio.direct.Dsl.Internal._
import zio.direct.core.util.Messages

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

  val spec = suite("VariaSpec") {
    suite("odd placements of defer/run") {
      test("defer in defer") {
        val out =
          defer {
            val v = defer {
              val env = ZIO.service[ConfigInt].run
              env
            }
            v.run.value + 1
          }
        assertZIO(out.provide(ZLayer.succeed(ConfigInt(3))))(equalTo(4))
      }
      +
      test("four services") {
        val out =
          defer {
            val (x, y) = (ZIO.service[Config1].run.value, ZIO.service[Config2].run.value)
            val config = ZIO.service[Config3].run
            x + config.value + y + ZIO.service[Config4].run.value
          }
        val provided =
          out.provide(
            ZLayer.succeed(Config1(1)),
            ZLayer.succeed(Config2(2)),
            ZLayer.succeed(Config3(3)),
            ZLayer.succeed(Config4(4))
          )
        assertZIO(provided)(equalTo(10))
      }
      +
      test("services with match statement") {
        val out =
          defer {
            val configValue =
              ZIO.service[Config1].run match {
                case Config1(value) => value + ZIO.service[Config2].run.value
              }
            configValue + ZIO.service[Config3].run.value
          }
        val provided =
          out.provide(
            ZLayer.succeed(Config1(1)),
            ZLayer.succeed(Config2(2)),
            ZLayer.succeed(Config3(4))
          )
        assertZIO(provided)(equalTo(7))
      }
      +
      test("using scope") {
        val out =
          defer {
            val value = ZIO.acquireRelease(ZIO.succeed(SomeService.open()))(svc => ZIO.succeed(svc.close())).run
            value.isOpen
          }
        val withScope =
          scoped { out }
        assertZIO(withScope)(equalTo(true))
      }
      +
      test("double tuple deconstruct") {
        val out =
          defer {
            val (x, y) = (ZIO.succeed("foo").run, ZIO.succeed("bar").run)
            val (x1, y1) = (ZIO.succeed("A" + x).run, ZIO.succeed("B" + y).run)
            x + x1 + y + y1
          }
        assertZIO(out)(equalTo("fooAfoobarBbar"))
      }
      +
      test("multi zios in run") {
        val out = defer {
          if (
            runBlock({
              for {
                env <- ZIO.service[ConfigInt]
                value <- ZIO.succeed(env.value)
              } yield (value)
            }) == 123
          )
            "foo"
          else
            "bar"
        }
        val providedA =
          out.provide(
            ZLayer.succeed(ConfigInt(123))
          )
        val providedB =
          out.provide(
            ZLayer.succeed(ConfigInt(456))
          )
        for {
          a <- providedA
          b <- providedB
        } yield {
          assert(a)(equalTo("foo")) && assert(b)(equalTo("bar"))
        }
      }
      +
      test("catch run inside run") {
        val msg = Messages.RunRemainingAfterTransformer
        runLiftFailMsg(msg) {
          """
          val x = succeed(123).run
          val y =
            ignore {
              succeed(456).run
            }
          x + y
          """
        }
      }
      +
      test("disallow mutable collection use") {
        import scala.collection.mutable.ArrayBuffer
        val v = new ArrayBuffer[Int](4)
        runLiftFailMsg(Messages.MutableCollectionDetected) {
          """
          v += 4
          """
        }
      }
      +
      test("disallow mutable collection use - declare but not use") {
        import scala.collection.mutable.ArrayBuffer
        runLiftFailMsg(Messages.MutableCollectionDetected) {
          """
          val v = new ArrayBuffer[Int](4)
          ZIO.succeed(123).run
          """
        }
      }
      +
      test("disallow implicit defs") {
        runLiftFailMsg(Messages.ImplicitsNotAllowed) {
          """
          implicit val x = 123
          x
          """
        }
      }
      +
      test("deconstruct and import") {
        class Blah(val value: Int)
        runLiftTest(4) {
          val (a, a1) = runBlock(ZIO.succeed((1, 2)))
          val blah = new Blah(3)
          import blah._
          val b = runBlock(ZIO.succeed(value))
          a + b
        }
      }
      +
      test("deconstruct and multiple import") {
        class Blah(val value: Int)
        class Blah0(val value0: Int)
        class Blah1(val value1: Int)
        runLiftTest(6) {
          val blah0 = new Blah0(1)
          import blah0._
          val blah1 = new Blah1(2)
          import blah1._
          val (a, a1) = ZIO.succeed((value0, value1)).run
          val blah = new Blah(3)
          import blah._
          val b = ZIO.succeed(value).run
          a + a1 + b
        }
      }
      +
      test("disallow implicit mutable use") {
        var x = 1
        runLiftFailMsg(Messages.MutableAndLazyVariablesNotAllowed) {
          """
          val y = x
          """
        }
      }
      +
      test("disallow implicit mutable use") {
        var x = 1
        runLiftFailMsg(Messages.DeclarationNotAllowed) {
          """
          lazy val x = 123
          """
        }
      }
    }
  }
}
