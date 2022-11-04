package zio.asyncawait.core.metaprog

import scala.quoted._
import zio.asyncawait.core.metaprog.Extractors._
import zio.asyncawait.core.util.Format

object Embedder {
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
    // alt: Trees.replaceIdent(using transformerQuotes)(body)(oldSymbol, newSymbolTerm.symbol)
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
        println(s"============+ Creating $oldSymbol:${Printer.TypeReprShortCode.show(oldSymbol.termRef.widen)} -> ${newSymbolTerm.show}:${Printer.TypeReprShortCode.show(newSymbolTerm.tpe.widen)} replacement let:\n${Format(Printer.TreeShortCode.show(out))}")
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
          '{ val m: t = ???; ${useSymbol(('m).asTerm).asExpr} }.asTerm.underlyingArgument match
            case Block(
              (valdef @ ValDef(_, _, _)) :: Nil,
              body
            ) =>
              (valdef.symbol, body)

    (symbol, body)
  }
}
