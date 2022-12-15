package zio.direct.core

import scala.reflect.macros.whitebox.Context
import zio.direct.core.Transformer
import zio.direct.core.metaprog.Instructions
import zio.direct.core.metaprog.InfoBehavior
import zio.direct.core.metaprog.WithUseParser
import scala.util.matching.Regex
import zio.ZIO

class Macro(val c: Context) extends Transformer with WithUseParser {
  import c.universe._

  def deferImpl[T](value: Expr[T])(tt: c.WeakTypeTag[T]): Expr[ZIO[_, _, _]] =
    c.Expr[ZIO[_, _, _]](apply(value.tree, Instructions.default.copy(info = InfoBehavior.Verbose)))
  // def deferWithUse[T](use: Regex)(value: Tree): Tree = {
  //   // val instr = RefineInstructions.fromUseTree(use, Instructions.default)
  //   // apply(value, Instructions.default)
  //   ???
  // }
}
