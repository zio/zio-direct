package zio.asyncawait

import scala.quoted._
import zio._
import zio.test._
import zio.asyncawait._

trait AsyncAwaitSpec extends ZIOSpecDefault {
  inline def runLiftTest[T](expected: T)(inline body: T) = {
    val asyncBody = async(body)
    assertZIO(asyncBody)(Assertion.equalTo(expected))
  }
}
