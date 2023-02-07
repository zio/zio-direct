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
  case class MyState(value: String)
  val init = MyState("init")

  val dw = deferWith[String, MyState]
  import dw._

  val e = new Exception("blah")
  val e1 = new Exception("blahblah")

  val spec = suite("VariaSpec")(
    // test("Simple Sequence") {
    //   val out =
    //     deferWithParams[Nothing, Any] {
    //       val a = ZPure.succeed(1)
    //       val b = ZPure.succeed("foo")
    //       (a.eval, b.eval)
    //     }
    //   assertIsType[ZPure[Nothing, Any, Any, Any, Nothing, (Int, String)]](out) andAssert
    //     assert(out.run)(equalTo((1, "foo")))
    // },
    // test("Simple Sequence with State") {
    //   val out =
    //     defer {
    //       val s1 = ZPure.get[MyState].eval.value
    //       val a = ZPure.succeed(s1).eval
    //       ZPure.set(MyState("foo")).eval
    //       val b = ZPure.succeed("bar").eval
    //       val s2 = ZPure.get[MyState].eval.value
    //       (s1, a, b, s2)
    //     }

    //   assertIsType[ZPure[String, MyState, MyState, Any, Nothing, (String, String, String, String)]](out) andAssert
    //     assert(out.provideState(MyState("init")).run)(equalTo(("init", "init", "bar", "foo")))
    // },
    // test("Simple Sequence with State - using primitives and logging") {
    //   val out =
    //     defer {
    //       val s1 = State.get().value
    //       val a = ZPure.succeed(s1).eval // ..
    //       log(a)
    //       State.set(MyState("foo"))
    //       val b = ZPure.succeed("bar").eval
    //       log(b)
    //       val s2 = State.get().value
    //       (s1, a, b, s2)
    //     }

    //   // Transparent inline screw up performance!!!
    //   // need to have notion of custom-eval functions i.e. things are treated as `eval` functions and go into the @monad
    //   assertIsType[ZPure[String, MyState, MyState, Any, Nothing, (String, String, String, String)]](out) andAssert //////
    //     assert(out.runAll(MyState("init")))(equalTo( ///
    //       (Chunk("init", "bar"), Right((MyState("foo"), ("init", "init", "bar", "foo"))))))
    // },
    // test("Impure/Impure If-statement") {
    //   val out = defer {
    //     if (ZPure.succeed(2).eval == 2)
    //       val v = ZPure.succeed[MyState, String]("foo").eval
    //       State.set(MyState(v))
    //       v
    //     else
    //       val v = ZPure.succeed[MyState, String]("bar").eval
    //       State.set(MyState(v))
    //       v
    //   }
    //   assert(out.run(MyState("init")))(equalTo((MyState("foo"), "foo")))
    // },
    // test("Impure/Impure Pat-match") {
    //   val out =
    //     defer {
    //       ZPure.succeed("a").eval match {
    //         case "a" => ZPure.succeed(1)
    //         case "b" => ZPure.succeed(2)
    //       }
    //     }
    //   assert(out.run(init))(equalTo((init, 1)))
    // },
    test("Try/Catch succeed") {
      val foo = "foo"

      // Further Up:
      // val dw = deferWith[String, MyState]
      // import dw._

      val out =
        defer {
          try {
            foo //// // // // // //
          } catch {
            case _ => foo
          }
        }
      // assertTrue(true)
      assertIsType[ZPure[String, MyState, MyState, Any, Nothing, String]](out) andAssert // // // //
        assert(out.run(init))(equalTo(Chunk(1)))
    }
    // test("Try/Catch caught") {
    //   val out =
    //     defer {
    //       try {
    //         val num = ZStream(1, 2, 3, 4).each
    //         if (num == 3) {
    //           throw new FooError
    //         } else {
    //           num
    //         }
    //       } catch {
    //         case _: FooError => ZStream(33, 44, 55).each
    //       }
    //     }
    //   assertIsType[ZStream[Any, FooError, Int]](out) andAssert
    //     assertZIO(out.runCollect)(equalTo(Chunk(1, 2, 33, 44, 55)))
    // },
    // test("Try/Catch NOT caught") {
    //   val out =
    //     defer {
    //       try {
    //         val num = ZStream(1, 2, 3, 4).each
    //         if (num == 3) {
    //           throw new FooError
    //         } else {
    //           num
    //         }
    //       } catch {
    //         case _: BarError => ZStream(33, 44, 55).each
    //       }
    //     }
    //   assertIsType[ZStream[Any, FooError, Int]](out) andAssert
    //     assertZIO(out.runCollect.exit)(fails(isSubtype[FooError](anything)))
    // },
    // test("Throw-fail") {
    //   val out =
    //     defer(Use.withNoCheck) {
    //       throw new FooError
    //     }
    //   assertZIO(out.runCollect.exit)(fails(isSubtype[FooError](anything)))
    // },
    // test("Throw-die") {
    //   val out =
    //     defer(Use.withNoCheck) {
    //       throwFoo()
    //     }
    //   assertZIO(out.runCollect.exit)(dies(isSubtype[FooError](anything)))
    // },
    // test("List Impure, body Impure") {
    //   var v = 1
    //   val out =
    //     defer(Use.withLenientCheck) {
    //       for (i <- ZStream(List(1, 2, 3), List(4, 5, 6)).each) {
    //         ZStream(v += i).each
    //       }
    //     }
    //   assertZIO(out.runCollect)(equalTo(Chunk((), ()))) andAssert
    //     assert(v)(equalTo(22))
    // },
    // test("Complex Example Case") {
    //   val x = ZStream(1, -2, -3)
    //   val y = ZStream("ab", "cde")
    //   val out =
    //     defer {
    //       val xx = x.each
    //       xx + (
    //         if xx > 0 then y.each.length() * x.each
    //         else y.each.length()
    //       )
    //     }
    //   // the above statement does not actually expand to this but they should be equivalent
    //   val compare =
    //     x.flatMap { xx =>
    //       if (xx > 0) {
    //         y.flatMap { yEach =>
    //           x.map { xEach =>
    //             xx + yEach.length * xEach
    //           }
    //         }
    //       } else {
    //         y.map { yEach =>
    //           xx + yEach.length
    //         }
    //       }
    //     }
    //   val expectedOutput = Chunk(3, -3, -5, 4, -5, -8, 0, 1, -1, 0)
    //   for {
    //     a <- out.runCollect
    //     b <- compare.runCollect
    //   } yield {
    //     assert(a)(equalTo(b)) && assert(a)(equalTo(expectedOutput))
    //   }
    // }
  )
}
