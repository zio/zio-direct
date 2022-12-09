package zio.direct.core.testing

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import zio.test._
import zio.ZIO
import zio.direct.core.Transformer
import zio.direct.core.metaprog.Instructions

private[direct] trait TestSupport {
  def runLiftTest[T](expected: T)(body: T): ZIO[Any, Nothing, TestResult] = macro TestSupportMacro.runLiftTest[T]
  def isType[T](input: T): Boolean = macro TestSupportMacro.isType[T]
  // def assertIsType[T](input: T): Assert = macro TestSupportMacro.assertIsType[T]
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

  def isType[T](input: Expr[T])(implicit tt: WeakTypeTag[T]): Expr[Boolean] = {
    val expectedTpe = weakTypeOf[T]
    val actualType = input.tree.tpe
    if (expectedTpe.widen =:= actualType.widen) {
      c.Expr[Boolean](q"true")
    } else {
      c.warning(c.enclosingPosition, s"Expected type to be: ${Format.Type(expectedTpe)} but got: ${Format.Type(actualType)}")
      c.Expr[Boolean](q"false")
    }
  }

  def runLiftTest[T](expected: Tree)(body: Tree): Tree = {
    val deferBody = apply(body, Instructions.default)
    q"""
    $deferBody.map { v =>
      assert(v)(zio.test.Assertion.equalTo($expected))
    }
    """
  }
}
