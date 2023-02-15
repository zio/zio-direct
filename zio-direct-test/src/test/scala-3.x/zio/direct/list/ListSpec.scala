package zio.direct.list

import zio.direct.{run => runBlock}
import zio.test._
import zio.test.Assertion._
import zio.direct.DeferRunSpec
import zio.direct.Use
import zio.direct.FooError
import zio.direct.BarError

object ListSpec extends DeferRunSpec {

  val e = new Exception("blah")
  val e1 = new Exception("blahblah")

  def spec =
    suite("ListSpec")(
      test("Maps and FlatMaps") {
        val out =
          select {
            val a = from(List(1, 2, 3))
            val b = from(List("foo", "bar"))
            (a, b)
          }
        val compare =
          for {
            a <- List(1, 2, 3)
            b <- List("foo", "bar")
          } yield (a, b)
        assert(out)(equalTo(compare))
      },
      test("Complex Example Case") {
        val x = List(1, -2, -3)
        val y = List("ab", "cde")
        val out =
          select {
            val xx = from(x)
            xx + (
              if xx > 0 then from(y).length() * from(x)
              else from(y).length()
            )
          }
        // the above statement does not actually expand to this but they should be equivalent
        val compare =
          x.flatMap { xx =>
            if (xx > 0) {
              y.flatMap { yEach =>
                x.map { xEach =>
                  xx + yEach.length * xEach
                }
              }
            } else {
              y.map { yEach =>
                xx + yEach.length
              }
            }
          }
        val expectedOutput = List(3, -3, -5, 4, -5, -8, 0, 1, -1, 0)
        assert(out)(equalTo(compare)) && assert(out)(equalTo(expectedOutput))
      },
      test("Throws a real exception") {
        def out =
          select {
            val a = from(List(1, 2))
            throw FooError()
            val b = from(List(3, 4))
            (a, b)
          }
        assert(out)(throwsA[FooError])
      },
      test("Catches a real exception and runs finalizers") {
        var v = 0
        val out =
          select(Use.withLenientCheck) {
            val a = from(List(1, 2))
            val b =
              try {
                throw FooError()
              } catch {
                case foo: FooError => from(List("foo", "bar"))
              } finally {
                v = 1
              }
            (a, b)
          }
        val compare =
          for {
            a <- List(1, 2)
            b <- List("foo", "bar")
          } yield (a, b)
        assert(out)(equalTo((compare))) && assert(v)(equalTo(1))
      }
    )
}
