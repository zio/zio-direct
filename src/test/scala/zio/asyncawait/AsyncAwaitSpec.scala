package zio.asyncawait

import scala.quoted._
import zio._
import zio.test._
import zio.test.Assertion._
import zio.asyncawait._
import scala.compiletime.testing.typeCheckErrors

trait AsyncAwaitSpec extends ZIOSpecDefault {
  val errorMsg =
    "Detected an `await` call inside of an unsupported structure"

  inline def runLiftTest[T](expected: T)(inline body: T) = {
    val asyncBody = async.verbose(body)
    // Not sure why but If I don't cast to .asInstanceOf[ZIO[Any, Nothing, ?]]
    // zio says it expects a layer of scala.Nothing
    assertZIO(asyncBody)(Assertion.equalTo(expected))
  }

  transparent inline def runLiftFailMsg(errorStringContains: String)(body: String) = {
    val errors =
      typeCheckErrors("async {" + body + "}").map(_.message)
    //val contains = errors.exists(error => error.message.contains(errorStringContains))
    assert(errors)(exists(containsString(errorMsg)))
    &&
    assert(errors)(exists(containsString(errorStringContains)))
  }

  transparent inline def runLiftFail(body: String) = {
    val errors =
      typeCheckErrors("async {" + body + "}").map(_.message)
    //val contains = errors.exists(error => error.message.contains(errorStringContains))
    assert(errors)(exists(containsString(errorMsg)))
  }
}
