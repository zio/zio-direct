package zio.direct.core

import scala.reflect.macros.whitebox.Context
import zio.direct.core.Transformer
import zio.direct.core.metaprog.Instructions
import zio.direct.core.metaprog.InfoBehavior
import zio.direct.core.metaprog.WithUseParser

class Macro(val c: Context) extends Transformer with WithUseParser {
  import c.universe.Tree

  def defer[T](value: Tree): Tree =
    apply(value, Instructions.default)
  def info[T](value: Tree): Tree =
    apply(value, Instructions.default.copy(info = InfoBehavior.Info))
  def verbose[T](value: Tree): Tree =
    apply(value, Instructions.default.copy(info = InfoBehavior.Verbose))
  def verboseTree[T](value: Tree): Tree =
    apply(value, Instructions.default.copy(info = InfoBehavior.VerboseTree))

  def deferWithUse[T](use: Tree)(value: Tree): Tree = {
    val instr = RefineInstructions.fromUseTree(use, Instructions.default)
    apply(value, instr)
  }
  def infoWithUse[T](use: Tree)(value: Tree): Tree = {
    val instr = RefineInstructions.fromUseTree(use, Instructions.default.copy(info = InfoBehavior.Info))
    apply(value, instr)
  }
  def verboseWithUse[T](use: Tree)(value: Tree): Tree = {
    val instr = RefineInstructions.fromUseTree(use, Instructions.default.copy(info = InfoBehavior.Verbose))
    apply(value, instr)
  }
  def verboseTreeWithUse[T](use: Tree)(value: Tree): Tree = {
    val instr = RefineInstructions.fromUseTree(use, Instructions.default.copy(info = InfoBehavior.VerboseTree))
    apply(value, instr)
  }
}
