package zio.direct

import scala.quoted._
import zio._
import zio.test._
import zio.test.Assertion._
import zio.direct._
import scala.compiletime.testing.typeCheckErrors
import zio.direct.core.metaprog.Verify
import zio.direct.core.metaprog.Collect
import zio.direct.Dsl.Params

trait AsyncAwaitSpec extends ZIOSpecDefault {
  // various config parameters to test zio dependency
  case class ConfigInt(value: Int)
  case class ConfigString(value: String)
  case class ConfigBool(value: Boolean)

  val errorMsg =
    "Detected an `await` call inside of an unsupported structure"

  inline def runLiftTest[T](expected: T)(inline body: T) = {
    val deferBody = defer(body)
    // Not sure why but If I don't cast to .asInstanceOf[ZIO[Any, Nothing, ?]]
    // zio says it expects a layer of scala.Nothing
    assertZIO(deferBody.asInstanceOf[ZIO[Any, ?, ?]])(Assertion.equalTo(expected))
  }

  inline def runLiftTestLenient[T](expected: T)(inline body: T) = {
    val deferBody = defer(Params(Verify.Lenient))(body)
    // Not sure why but If I don't cast to .asInstanceOf[ZIO[Any, Nothing, ?]]
    // zio says it expects a layer of scala.Nothing
    assertZIO(deferBody.asInstanceOf[ZIO[Any, ?, ?]])(Assertion.equalTo(expected))
  }

  transparent inline def runLiftFailLenientMsg(errorStringContains: String)(body: String) = {
    val errors =
      typeCheckErrors("defer(zio.direct.Dsl.Params(zio.direct.core.metaprog.Verify.Lenient)) {" + body + "}").map(_.message)
    assert(errors)(exists(containsString(errorStringContains)))
  }

  transparent inline def runLiftFailMsg(errorStringContains: String)(body: String) = {
    val errors =
      typeCheckErrors("defer({" + body + "})").map(_.message)
    assert(errors)(exists(containsString(errorStringContains)))
  }

  transparent inline def runLiftFail(body: String) = {
    val errors =
      typeCheckErrors("defer {" + body + "}").map(_.message)
    assert(errors)(isNonEmpty)
  }
}
