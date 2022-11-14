package zio.run.core.metaprog

import scala.quoted._
import zio.run.core.metaprog.Extractors._
import zio.run.core.util.Format

object Embedder {

  def topLevelOwner(using Quotes): quotes.reflect.Symbol =
    import quotes.reflect._
    // Need to check the name because for some reason checking symbol flags recurisvely upward gives you
    // a |dotty.tools.dotc.core.CyclicReference error.
    findOwner(Symbol.spliceOwner, sym => sym.name == "macro")

  object ZioApply {
    def apply(using Quotes)(term: quotes.reflect.Term) =
      import quotes.reflect._
      term.tpe.widen.asType match
        case '[t] =>
          '{ zio.ZIO.succeed[t](${ term.asExprOf[t] }) }

    def True(using Quotes) =
      import quotes.reflect._
      apply(Expr(true).asTerm).asExprOf[zio.ZIO[Any, Nothing, Boolean]]

    def False(using Quotes) =
      import quotes.reflect._
      apply(Expr(false).asTerm).asExprOf[zio.ZIO[Any, Nothing, Boolean]]
  }

  private def findOwner(using Quotes)(owner: quotes.reflect.Symbol, skipSymbol: quotes.reflect.Symbol => Boolean): quotes.reflect.Symbol =
    var topOwner = owner
    while (skipSymbol(topOwner)) topOwner = topOwner.owner
    topOwner

  // TODO Maybe a better name? Alternative we can actually search for the old symbol and replace
  // it but that is much worse for performance
  /**
   * In a case where we have:
   *  val a = unlift(foobar)
   *  otherStuff
   *
   * We can either lift that into:
   *  unlift(foobar).flatMap { v => (otherStuff /*with replaced a -> v*/) }
   *
   * Or we can just do
   *  unlift(foobar).flatMap { v => { val a = v; otherStuff /* with the original a variable*/ } }
   *
   * I think the 2nd variant more performant but keeping 1st one (Trees.replaceIdent(...)) around for now.
   */
  def replaceSymbolIn(using Quotes)(in: quotes.reflect.Term)(oldSymbol: quotes.reflect.Symbol, newSymbolTerm: quotes.reflect.Term) =
    // alt: Trees.replaceIdent(using macroQuotes)(body)(oldSymbol, newSymbolTerm.symbol)
    import quotes.reflect._
    BlockN(List(
      ValDef(oldSymbol, Some(newSymbolTerm)),
      in
    ))

  def replaceSymbolInBodyMaybe(using Quotes)(body: quotes.reflect.Term)(oldSymbolTerm: Option[quotes.reflect.Symbol], newSymbolTerm: quotes.reflect.Term) =
    import quotes.reflect._
    oldSymbolTerm match
      case Some(oldSymbol) =>
        val out = replaceSymbolIn(body)(oldSymbol, newSymbolTerm)
        // println(s"============+ Creating $oldSymbol:${Printer.TypeReprShortCode.show(oldSymbol.termRef.widen)} -> ${newSymbolTerm.show}:${Printer.TypeReprShortCode.show(newSymbolTerm.tpe.widen)} replacement let:\n${Format(Printer.TreeShortCode.show(out))}")
        out
      case None =>
        body

  def useNewSymbolIn(using Quotes)(tpe: quotes.reflect.TypeRepr)(useSymbol: quotes.reflect.Term => quotes.reflect.Term) = {
    import quotes.reflect._
    // TODO Try to write this using ValDef.let(...). Might be more efficient
    val (symbol, body) =
      tpe.asType match
        case '[t] =>
          // TODO get rid of underlyingArgument. Should only need one top-level Uninline
          '{ val m: t = ???; ${ useSymbol(('m).asTerm).asExpr } }.asTerm.underlyingArgument match
            case Block(
                  (valdef @ ValDef(_, _, _)) :: Nil,
                  body
                ) =>
              (valdef.symbol, body)

    (symbol, body)
  }
}
