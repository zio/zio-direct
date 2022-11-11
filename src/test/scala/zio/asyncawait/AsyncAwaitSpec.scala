package zio.asyncawait

import scala.quoted._
import zio._
import zio.test._
import zio.test.Assertion._
import zio.asyncawait._
import scala.compiletime.testing.typeCheckErrors
import zio.asyncawait.core.metaprog.Verify
import zio.asyncawait.core.metaprog.Collect

trait AsyncAwaitSpec extends ZIOSpecDefault {
  val errorMsg =
    "Detected an `await` call inside of an unsupported structure"

  inline def runLiftTest[T](expected: T)(inline body: T) = {
    val asyncBody = async(body)
    // Not sure why but If I don't cast to .asInstanceOf[ZIO[Any, Nothing, ?]]
    // zio says it expects a layer of scala.Nothing
    assertZIO(asyncBody.asInstanceOf[ZIO[Any, ?, ?]])(Assertion.equalTo(expected))
  }

  inline def runLiftTestLenient[T](expected: T)(inline body: T) = {
    val asyncBody = async(Collect.Sequence, Verify.Lenient)(body)
    // Not sure why but If I don't cast to .asInstanceOf[ZIO[Any, Nothing, ?]]
    // zio says it expects a layer of scala.Nothing
    assertZIO(asyncBody.asInstanceOf[ZIO[Any, ?, ?]])(Assertion.equalTo(expected))
  }

  transparent inline def runLiftFailLenientMsg(errorStringContains: String)(body: String) = {
    val errors =
      typeCheckErrors("async(zio.asyncawait.core.metaprog.Collect.Sequence, zio.asyncawait.core.metaprog.Verify.Lenient) {" + body + "}").map(_.message)
    assert(errors)(exists(containsString(errorStringContains)))
  }

  transparent inline def runLiftFailMsg(errorStringContains: String)(body: String) = {
    val errors =
      typeCheckErrors("async({" + body + "})").map(_.message)
    assert(errors)(exists(containsString(errorStringContains)))
  }

  transparent inline def runLiftFail(body: String) = {
    val errors =
      typeCheckErrors("async {" + body + "}").map(_.message)
    assert(errors)(isNonEmpty)
  }
}
