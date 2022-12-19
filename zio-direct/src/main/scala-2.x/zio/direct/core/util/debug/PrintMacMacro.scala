package zio.direct.core.util.debug

import scala.reflect.macros.whitebox.{Context => MacroContext}
import zio.direct.core.util.WithFormat
import zio.direct.core.metaprog.Trees

class PrintMacMacro(val c: MacroContext) extends WithFormat {
  import c.universe._

  def apply(valueRaw: Tree): Tree = {
    println(
      "================= Printing Tree =================\n" +
        show(valueRaw)
    )
    q"()"
  }

  def detail(valueRaw: Tree): Tree = {
    val value = c.typecheck(valueRaw)
    println(
      "================= Printing Tree =================\n" +
        show(value) + "\n" +
        "================= Printing Tree =================\n" +
        Format(showRaw(value))
    )

    Trees.traverse(c)(value) {
      // case v: ValDef =>
      //   println(s"========= ${show(v)} - isLazy: ${v.mods.hasFlag(Flag.LAZY)} - isMutable: ${v.mods.hasFlag(Flag.MUTABLE)} - isImplicit: ${v.mods.hasFlag(Flag.IMPLICIT)}")
      case AssignOrNamedArg(lhs, _) =>
        println(s"========= ${lhs} - is a paramter: ${lhs.symbol.isParameter} - is mutable ${lhs.symbol.isTerm && lhs.symbol.asTerm.isVar}")
    }

    q"()"
  }

}
