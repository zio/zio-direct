package zio.direct.core.testing

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import zio.test._
import zio.ZIO
import zio.direct.core.Transformer
import zio.direct.core.metaprog.Instructions
import scala.reflect.macros.TypecheckException
import scala.reflect.macros.ParseException
import zio.direct.core.metaprog.Verify
import zio.direct.testing.TestSupportRuntime

private[direct] trait TestSupport extends TestSupportRuntime {
  def runLiftTest[T](expected: T)(body: T): ZIO[Any, Any, TestResult] = macro TestSupportMacro.runLiftTest[T]
  def runLiftTestLenient[T](expected: T)(body: T): ZIO[Any, Any, TestResult] = macro TestSupportMacro.runLiftTestLenient[T]
  def runLiftFail[T](body: T): TestResult = macro TestSupportMacro.runLiftFail[T]
  def runLiftFailMsg[T](msg: String)(body: T): TestResult = macro TestSupportMacro.runLiftFailMsg[T]
  def runLiftFailLenientMsg[T](msg: String)(body: T): TestResult = macro TestSupportMacro.runLiftFailLenientMsg[T]

  def isType[T](input: Any): Boolean = macro TestSupportMacro.isType[T]
  def assertIsType[T](input: Any): TestResult = macro TestSupportMacro.assertIsType[T]
}

private[direct] class TestSupportMacro(val c: Context) extends Transformer {
  import c.universe._

  def sourceLocationTree: Tree = {
    // Symbol.spliceOwner.pos.map(p => (p.sourceFile.path, p.startLine)).getOrElse("", 0)
    val pos = c.enclosingPosition.pos
    val name = pos.source.path
    val line = pos.line
    q"SourceLocation($name, $line)"
  }

  def assertIsType[T](input: Expr[T])(implicit tt: WeakTypeTag[T]): Expr[TestResult] = {
    c.Expr[TestResult](q"assertTrue(${isType(input)})")
  }

  def isType[T](input: Expr[T])(implicit tt: WeakTypeTag[T]): Expr[Boolean] = {
    val expectedTpe = weakTypeOf[T]
    val actualType = c.typecheck(input.tree).tpe
    if (expectedTpe =:= actualType) {
      c.Expr[Boolean](q"true")
    } else {
      c.warning(c.enclosingPosition, s"Expected type to be: ${Format.Type(expectedTpe)} but got: ${Format.Type(actualType)}")
      c.Expr[Boolean](q"false")
    }
  }

  def runLiftTest[T](expected: Tree)(body: Tree): Tree = {
    val deferBody = apply(body, Instructions.default)
    val v = freshName("v")
    q"""
    $deferBody.map { ${toVal(v)} =>
      assert($v)(zio.test.Assertion.equalTo($expected))
    }
    """
  }

  def runLiftTestLenient[T](expected: Tree)(body: Tree): Tree = {
    val deferBody = apply(body, Instructions.default.copy(verify = Verify.Lenient))
    val v = freshName("v")
    q"""
    $deferBody.map { ${toVal(v)} =>
      assert($v)(zio.test.Assertion.equalTo($expected))
    }
    """
  }

  private def doTypecheck(expression: String) = {
    val output =
      try {
        Right(c.typecheck(c.parse("{ " + expression + " }")))
      } catch {
        case e: TypecheckException =>
          Left(e.getMessage())
        case e: ParseException =>
          Left(e.getMessage())
      }
    output match {
      case Right(tree) => (false, s"<NO FAILURE> \nCode:${Format(Format.Tree(tree))}")
      case Left(msg)   => (true, msg)
    }
  }

  private def wrapDefer(body: String, verify: Verify = Verify.Strict) = {
    verify match {
      case Verify.Lenient =>
        "defer(zio.direct.Use.withLenientCheck) {\n" + body + "\n}"
      case Verify.None =>
        "defer(zio.direct.Use.withNoCheck) {\n" + body + "\n}"
      case Verify.Strict =>
        "defer {\n" + body + "\n}"
    }
  }

  private def requireConstString(body: Tree) = {
    body match {
      case q"${str: String}" =>
        str
      case _ =>
        c.abort(body.pos, s"Expression needs to be a constant-string to be type-checkable but was:\n${show(body)}")
    }
  }

  def runLiftFail[T](body: Tree): Tree = {
    val bodyStr = requireConstString(body)
    val (fails, failMsg) = doTypecheck(wrapDefer(bodyStr))
    q"zio.test.assertTrue($fails)"
  }

  def runLiftFailMsg[T](msg: Tree)(body: Tree): Tree = {
    val bodyStr = requireConstString(body)
    val (fails, failMsg) = doTypecheck(wrapDefer(bodyStr))
    q"zio.test.assertTrue($fails) && zio.test.assert($failMsg)(zio.test.Assertion.containsString($msg))"
  }

  def runLiftFailLenientMsg[T](msg: Tree)(body: Tree): Tree = {
    val bodyStr = requireConstString(body)
    val (fails, failMsg) = doTypecheck(wrapDefer(bodyStr, Verify.Lenient))
    q"zio.test.assertTrue($fails) && zio.test.assert($failMsg)(zio.test.Assertion.containsString($msg))"
  }
}
