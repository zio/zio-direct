package zio.direct.core.util.debug

import zio.direct.core.metaprog.Extractors._
import zio.direct.core.util.Format

import scala.quoted._
import zio.direct.core.metaprog.Trees
import zio.direct.core.util.ShowDetails

object PrintMac {

  inline def apply(inline any: Any): Unit = ${ printMacImpl('any, '{ false }) }
  inline def detail(inline any: Any): Unit = ${ printMacImpl('any, '{ true }) }
  inline def passthrough[T](inline any: T): T = ${ printMacImpl('any, '{ false }) }

  def printMacImpl[T: Type](anyRaw: Expr[T], showDetailRaw: Expr[Boolean])(using Quotes): Expr[T] = {
    import quotes.reflect._
    val showDetail = Expr.unapply(showDetailRaw).getOrElse { report.errorAndAbort("showDetail must be a constant value true/false") }

    val any = anyRaw.asTerm.underlyingArgument.asExprOf[T]
    val deser = any.asTerm

    println("================= Tree =================")
    println(Format.Term(any.asTerm, Format.Mode.DottyColor(ShowDetails.Standard)))

    if (showDetail) {
      println("================= Detail =================")
      println(Format(Printer.TreeStructure.show(any.asTerm)))
    }

    any
  }
}
