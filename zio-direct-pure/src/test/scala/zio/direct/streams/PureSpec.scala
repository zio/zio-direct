package zio.direct.pure

import zio.direct.DeferRunSpec
import zio.test._
import zio.test.Assertion._
import zio.direct.Use
import zio.direct.core.util.Messages
import scala.annotation.nowarn
import zio.direct.DeferRunSpec
import zio.stream.ZStream
import zio.Chunk
import zio.prelude.fx.ZPure


object PureSpec extends DeferRunSpec {
  val dc = deferWith[String, MyState]
  import dc._
  val init = MyState("init")

  val e = new Exception("blah")
  val e1 = new Exception("blahblah")

  val spec = suite("VariaSpec")(
    test("Simple Sequence") {
      val out =
        deferWith[Nothing, Any].defer {
          val a = ZPure.succeed(1)
          val b = ZPure.succeed("foo")
          (a.eval, b.eval)
        }
      assertIsType[ZPure[Nothing, Any, Any, Any, Nothing, (Int, String)]](out) andAssert
        assert(out.run)(equalTo((1, "foo"))) //
    },
    test("Simple Sequence with State") {
      val out =
        defer {
          val s1 = ZPure.get[MyState].eval.value
          val a = ZPure.succeed[MyState, String](s1).eval
          ZPure.set(MyState("foo")).eval
          val b = ZPure.succeed[MyState, String]("bar").eval
          val s2 = ZPure.get[MyState].eval.value
          (s1, a, b, s2)
        }

      assertIsType[ZPure[String, MyState, MyState, Any, Nothing, (String, String, String, String)]](out) andAssert
        assert(out.provideState(MyState("init")).run)(equalTo(("init", "init", "bar", "foo")))
    },
    test("Simple Sequence with State - using primitives and logging") {
      val out = // ddd
        defer {
          val s1 = State.get().value
          val a = ZPure.succeed[MyState, String](s1).eval
          log(a)
          State.set(MyState("foo"))
          val b = ZPure.succeed[MyState, String]("bar").eval
          log(b)
          val s2 = State.get().value
          (s1, a, b, s2)
        }
      assert(out.runAll(MyState("init")))(equalTo(
        (Chunk("init", "bar"), Right((MyState("foo"), ("init", "init", "bar", "foo"))))
      ))
    },
    test("Impure/Impure If-statement") {
      val out = defer {
        if (ZPure.succeed[MyState, Int](2).eval == 2)
          val v = ZPure.succeed[MyState, String]("foo").eval
          State.set(MyState(v))
          v
        else
          val v = ZPure.succeed[MyState, String]("bar").eval
          State.set(MyState(v))
          v
      }
      assert(out.run(MyState("init")))(equalTo((MyState("foo"), "foo")))
    },
    test("Impure/Impure Pat-match") {
      val out =
        defer {
          ZPure.succeed[MyState, String]("a").eval match {
            case "a" => ZPure.succeed[MyState, Int](1).eval
            case "b" => ZPure.succeed[MyState, Int](2).eval
          }
        }
      assert(out.run(init))(equalTo((init, 1)))
    },
    test("Try/Catch caught") {
      val out =
        defer {
          try {
            val num = ZPure.succeed[MyState, Int](1).eval
            if (num == 1) {
              throw new FooError
            } else {
              num
            }
          } catch {
            case _: FooError => ZPure.succeed[MyState, Int](18).eval
          }
        }
      assertIsType[ZPure[String, MyState, MyState, Any, FooError, Int]](out) andAssert
        assert(out.catchAll(e => throw e).run(init))(equalTo((init, 18)))
    },
    test("Try/Catch NOT caught") {
      val fooError = new FooError
      val out =
        defer {
          try {
            val num = Wrap.succeed(3).eval
            if (num == 3) {
              throw fooError
            } else {
              num
            }
          } catch {
            case _: BarError => Wrap.succeed(33).eval
          }
        }
      assert(out.runAll(init))(equalTo(
        // NOTE that the Chunk() is the logging, not the init
        (Chunk(), Left(zio.prelude.fx.Cause(fooError)))
      ))
    },
    test("Throw-fail") {
      val fooError = new FooError
      val out =
        defer(Use.withNoCheck) {
          throw fooError
        }
      assert(out.runAll(init))(equalTo(
        (Chunk(), Left(zio.prelude.fx.Cause(fooError)))
      ))
    },
    test("List Impure, body Impure") {
      var v = 1
      val out =
        defer(Use.withLenientCheck) {
          for (i <- Wrap.succeed(List(1, 2, 3)).eval) {
            Wrap.succeed(v += i).eval
          }
        }
      // assert(out.provideState(init).run) doesn't run anything, possibly because the after the .run it's a Unit-type
      out.provideState(init).run
      assert(v)(equalTo(7))
    }
  )
}
