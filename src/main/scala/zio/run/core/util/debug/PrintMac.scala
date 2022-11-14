package zio.run.core.util.debug

import zio.run.core.metaprog.Extractors._
import zio.run.core.util.Format

import scala.quoted._
import zio.run.core.util.ZioFacade
import zio.run.core.metaprog.Trees

object PrintMac {

  inline def apply(inline any: Any, inline showDetail: Boolean = false, inline deserializeAst: Boolean = false): Unit = ${ printMacImpl('any, 'showDetail, 'deserializeAst) }
  inline def passthrough[T](inline any: T, inline showDetail: Boolean = false, inline deserializeAst: Boolean = false): T = ${ printMacImpl('any, 'showDetail, 'deserializeAst) }

  def printMacImpl[T: Type](anyRaw: Expr[T], showDetailRaw: Expr[Boolean], deserializeAstRaw: Expr[Boolean])(using Quotes): Expr[T] = {
    import quotes.reflect._
    val showDetail = Expr.unapply(deserializeAstRaw).getOrElse { report.throwError("showDetail must be a constant value true/false") }
    val deserializeAst = Expr.unapply(deserializeAstRaw).getOrElse { report.throwError("deserializeAst must be a constant value true/false") }

    val any = anyRaw.asTerm.underlyingArgument.asExprOf[T]
    val deser = ZioFacade.makeFacade(any.asTerm)

    println("================= Tree =================")
    println(Format(Printer.TreeShortCode.show(deser)))

    // println("================= Detail =================")
    // println(Format(Printer.TreeStructure.show(any.asTerm)))

    Trees.traverse(any.asTerm, Symbol.spliceOwner) {
      case v: ValDef => println(s"Flags: ${v.symbol.flags.is(Flags.Mutable)}")
    }

    // println("================= Pretty Tree =================")
    // println(pprint.apply(Untype(any.asTerm)))

    any
  }
}
