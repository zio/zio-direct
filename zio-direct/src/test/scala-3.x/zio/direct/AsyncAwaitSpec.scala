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
import zio.direct.core.util.Format
import zio.internal.stacktracer.SourceLocation

object DeferRunSpec {
  def sourceLocationImpl(using Quotes): Expr[SourceLocation] = {
    import quotes.reflect._
    val (name, line) = Symbol.spliceOwner.pos.map(p => (p.sourceFile.path, p.startLine)).getOrElse("", 0)
    '{ SourceLocation(${ Expr(name) }, ${ Expr(line) }) }
  }

  def isType[T: Type](intput: Expr[Any])(using Quotes): Expr[Boolean] =
    import quotes.reflect._
    val expectedTpe = TypeRepr.of[T]
    val actualType = intput.asTerm.tpe.widenTermRefByName
    if (expectedTpe =:= actualType)
      '{ true }
    else
      report.warning(s"Expected type to be: ${Format.TypeRepr(expectedTpe)} but got: ${Format.TypeRepr(actualType)}")
      '{ false }
}

trait DeferRunSpec extends ZIOSpecDefault {
  // various config parameters to test zio dependency
  case class ConfigInt(value: Int)
  case class ConfigString(value: String)
  case class ConfigBool(value: Boolean)
  case class ConfigT[T](value: T)
  case class ConfigList(value: List[Int])

  class FooError extends Exception("foo")
  def throwFoo() = throw new FooError
  def makeFooError = new FooError

  class BarError extends Exception("foo")
  def throwBar() = throw new BarError
  def makeBarError = new BarError

  val errorMsg =
    "Detected an `run` call inside of an unsupported structure"

  transparent inline def isType[T](inline input: Any) = ${ DeferRunSpec.isType[T]('input) }
  transparent inline def assertIsType[T](inline input: Any) = {
    assertTrue(isType[T](input))
  }

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

    // assert(errors)(exists(containsString(errorStringContains)))
    zio.test.UseSmartAssert.of(errors, None, None)(exists(containsString(errorStringContains)))(sourceLocation)
  }

  inline def sourceLocation: SourceLocation = ${ DeferRunSpec.sourceLocationImpl }

  transparent inline def runLiftFail(body: String) = {
    val expression = "defer {\n" + body + "\n}"
    val errors =
      typeCheckErrors(expression).map(_.message)

    zio.test.UseSmartAssert.of(errors, None, None)(Assertion.isNonEmpty)(sourceLocation)
  }
}
