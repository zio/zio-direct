package zio.direct

import scala.quoted._
import zio._
import zio.test._
import zio.test.Assertion._
import zio.direct._
import scala.compiletime.testing.typeCheckErrors
import zio.direct.core.metaprog.Verify
import zio.direct.core.metaprog.Collect
import zio.direct.core.util.Format
import zio.internal.stacktracer.SourceLocation

object DeferRunSpec {
  def sourceLocationImpl(using Quotes): Expr[SourceLocation] = {
    import quotes.reflect._
    val (name, line) = Symbol.spliceOwner.pos.map(p => (p.sourceFile.path, p.startLine)).getOrElse("", 0)
    '{ SourceLocation(${ Expr(name) }, ${ Expr(line) }) }
  }

  def isType[T: Type](input: Expr[Any])(using Quotes): Expr[Boolean] =
    import quotes.reflect._
    val expectedTpe = TypeRepr.of[T]
    val actualType = input.asTerm.tpe.widenTermRefByName
    if (expectedTpe =:= actualType)
      '{ true }
    else
      report.warning(s"Expected type to be: ${Format(Printer.TypeReprStructure.show(expectedTpe))} but got: ${Format(Printer.TypeReprStructure.show(actualType))}")
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

  def wantStringThrowFoo(): String = throw new FooError

  class BarError extends Exception("foo")
  def throwBar() = throw new BarError
  def makeBarError = new BarError

  implicit class TestZIOOps(result: ZIO[Any, _, TestResult]) {
    def andAssert(otherResult: TestResult) =
      for {
        r <- result
      } yield r && otherResult
  }

  implicit class TestZIOOps2(result: TestResult) {
    def andAssert(otherResult: ZIO[Any, _, TestResult]) =
      for {
        r <- otherResult
      } yield result && r
  }

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

    // Not sure why but this cases errors in BlockSpec. Need to debug zio-test to find out the issue
    // assertZIO(deferBody.asInstanceOf[ZIO[Any, ?, ?]])(Assertion.equalTo(expected))

    deferBody.asInstanceOf[ZIO[Any, ?, ?]].flatMap { v =>
      zio.test.UseSmartAssert.of(v, None, None)(Assertion.equalTo(expected))(sourceLocation)
    }
  }

  inline def runLiftTestLenient[T](expected: T)(inline body: T) = {
    val deferBody = defer.use(Use.withLenientCheck)(body)
    // Not sure why but If I don't cast to .asInstanceOf[ZIO[Any, Nothing, ?]]
    // zio says it expects a layer of scala.Nothing

    deferBody.asInstanceOf[ZIO[Any, ?, ?]].flatMap { v =>
      zio.test.UseSmartAssert.of(v, None, None)(Assertion.equalTo(expected))(sourceLocation)
    }
  }

  transparent inline def runLiftFailLenientMsg(errorStringContains: String)(body: String) = {
    val errors =
      typeCheckErrors("defer.use(zio.direct.Use.withLenientCheck) {" + body + "}").map(_.message)

    // assert(errors)(exists(containsString(errorStringContains)))
    zio.test.UseSmartAssert.of(errors, None, None)(exists(containsString(errorStringContains)))(sourceLocation)
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
