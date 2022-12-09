package zio.direct.core.testing

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import org.scalamacros.resetallattrs._
import zio.direct.core.metaprog.Trees
import zio.direct.core.util.WithFormat
import zio.direct.core.metaprog.WithIR
import zio.direct.core.util.WithUnsupported
import zio.test._

private[direct] trait TestSupport {
  def runLiftTest[T](expected: T)(body: T): Unit = macro TestSupportMacro.runLiftTest[T]
  def isType[T](input: T): Boolean = macro TestSupportMacro.isType[T]
  // def assertIsType[T](input: T): Assert = macro TestSupportMacro.assertIsType[T]
}

private[direct] class TestSupportMacro(val c: Context) extends WithIR with WithFormat with WithUnsupported {
  import c.universe._

  def showTree(t: Tree): Tree = {
    c.warning(c.enclosingPosition, t.toString)
    q"()"
  }
  def showRawTree(t: Tree): Tree = {
    c.warning(c.enclosingPosition, showRaw(t))
    q"()"
  }

  def forceLift(t: Tree): Tree =
    c.resetAllAttrs {
      Trees.Transform(c)(t) {
        case q"$pack.unlift[$t]($v)" =>
          q"${c.prefix}.get($v)"
      }
    }

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

  def runLiftTest[T](expected: Tree)(body: Tree): Tree =
    c.resetAllAttrs {
      val deferBody = q"zio.direct.defer($body)"
      val sourceLocation = sourceLocationTree
      // q"zio.test.UseSmartAssert.of(errors, None, None)(exists(containsString(errorStringContains)))($sourceLocation)"
      q"""
      $deferBody.flatMap { v =>
        zio.test.UseSmartAssert.of(v, None, None)(Assertion.equalTo($expected))($sourceLocation)
      }
      """
    }
}
