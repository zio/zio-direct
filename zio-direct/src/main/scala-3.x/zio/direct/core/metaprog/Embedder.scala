package zio.direct.core.metaprog

import scala.quoted._
import zio.direct.core.metaprog.Extractors._
import zio.direct.core.util.Format
import zio.ZIO

object Embedder {

  def computeCommonBaseClass(using Quotes)(a: quotes.reflect.TypeRepr, b: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect._
    // As an alterantive, can construct a statement similar to Scala2-zio-direct but
    // somehow this still seems to make a type union. Need to investigate
    // (a.asType, b.asType) match
    //   case ('[aTpe], '[bTpe]) =>
    //     '{
    //       if (true) { val av: aTpe = ???; av }
    //       else { val bv: bTpe = ???; bv }
    //     }.asTerm.tpe
    import scala.collection.immutable.ListSet
    if (a =:= TypeRepr.of[Nothing] && b =:= TypeRepr.of[Nothing]) TypeRepr.of[Nothing]
    else if (a =:= TypeRepr.of[Nothing]) b
    else if (b =:= TypeRepr.of[Nothing]) a
    else if (a =:= TypeRepr.of[Any] || b =:= TypeRepr.of[Any]) TypeRepr.of[Any]
    else
      val isectOpt = ListSet(a.widen.baseClasses: _*).intersect(ListSet(b.widen.baseClasses: _*)).headOption
      val isectTypeOpt = isectOpt.map(baseClassSymbol => baseClassSymbol.typeRef)
      isectTypeOpt.getOrElse(TypeRepr.of[Any])
  }

  def topLevelOwner(using Quotes): quotes.reflect.Symbol =
    import quotes.reflect._
    // Need to check the name because for some reason checking symbol flags recurisvely upward gives you
    // a |dotty.tools.dotc.core.CyclicReference error.
    findOwner(Symbol.spliceOwner, sym => sym.name == "macro")

  object ZioApply {
    // wrap the value in a ZIO.succeed. Note that widening here could have some serious consequences
    // and make statements not many any sense of the term passed in represents a singleton-type
    // e.g. Zio.Apply('{ 1 }) or a union of singleton types e.g. Zio.Apply('{ if (blah) 1 else 2 })
    // (the latter is typed as `1 | 2`)
    def succeed(using Quotes)(term: quotes.reflect.Term) =
      // TODO widenByTermRef just in case?
      import quotes.reflect._
      term.tpe.asType match
        case '[t] =>
          '{ zio.ZIO.succeed[t](${ term.asExprOf[t] }) } // .asInstanceOf[ZIO[Any, Nothing, t]]

    def True(using Quotes) =
      import quotes.reflect._
      succeed(Expr(true).asTerm).asExprOf[zio.ZIO[Any, Nothing, Boolean]]

    def False(using Quotes) =
      import quotes.reflect._
      succeed(Expr(false).asTerm).asExprOf[zio.ZIO[Any, Nothing, Boolean]]
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
    // Trees.replaceIdent(in)(oldSymbol, newSymbolTerm)
    import quotes.reflect._
    BlockN(List(
      ValDef(oldSymbol, Some(newSymbolTerm)),
      in
    ))

  def replaceSymbolInBodyMaybe(using Quotes)(body: quotes.reflect.Term)(oldSymbolTerm: Option[quotes.reflect.Symbol], newSymbolTerm: quotes.reflect.Term) =
    import quotes.reflect._
    oldSymbolTerm match
      case Some(oldSymbol) =>
        replaceSymbolIn(body)(oldSymbol, newSymbolTerm)
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
