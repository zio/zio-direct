package zio.asyncawait

import scala.quoted._
import zio._
import zio.test._
import zio.test.Assertion._
import zio.asyncawait._
import scala.compiletime.testing.typeCheckErrors

trait AsyncAwaitSpec extends ZIOSpecDefault {
  inline def runLiftTest[T](expected: T)(inline body: T) = {
    val asyncBody = async(body)
    assertZIO(asyncBody)(Assertion.equalTo(expected))
  }

  transparent inline def runLiftFail(inline errorStringContains: String) (body: String) = {
    val errors =
      typeCheckErrors("async {" + body + "}").map(_.message)
    //val contains = errors.exists(error => error.message.contains(errorStringContains))
    assert(errors)(exists(containsString(errorStringContains)))
  }
}
