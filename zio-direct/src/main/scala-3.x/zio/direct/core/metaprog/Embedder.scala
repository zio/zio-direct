package zio.direct.core.metaprog

import scala.quoted._
import zio.direct.core.metaprog.Extractors._
import zio.direct.core.metaprog.TypeUnion
import zio.direct.core.util.Format
import zio.ZIO
import zio.Chunk

object Embedder {

  object Compose {
    def andN(using Quotes)(types: Chunk[quotes.reflect.TypeRepr]) =
      import quotes.reflect._
      if (types.length == 1)
        types.head
      else if (types.length > 1)
        types.reduce(and(_, _))
      else
        TypeRepr.of[Any]

    def orN(using Quotes)(types: Chunk[quotes.reflect.TypeRepr])(implicit typeUnion: TypeUnion) = {
      import quotes.reflect._
      if (types.length == 1)
        types.head
      else if (types.length > 1)
        types.reduce(or(_, _)(typeUnion))
      else
        TypeRepr.of[Nothing]
    }

    def or(using Quotes)(a: quotes.reflect.TypeRepr, b: quotes.reflect.TypeRepr)(implicit typeUnion: TypeUnion) = {
      import quotes.reflect._
      typeUnion match {
        case TypeUnion.OrType =>
          (a.widen.asType, b.widen.asType) match
            case ('[at], '[bt]) =>
              TypeRepr.of[at | bt]
        case TypeUnion.LeastUpper =>
          Embedder.computeCommonBaseClass(a.widen, b.widen)
      }
    }

    def and(using Quotes)(a: quotes.reflect.TypeRepr, b: quotes.reflect.TypeRepr) =
      // if either type is Any, specialize to the thing that is narrower
      import quotes.reflect._
      val out =
        (a.widen.asType, b.widen.asType) match
          case ('[at], '[bt]) =>
            if (a =:= TypeRepr.of[Any] && b =:= TypeRepr.of[Any])
              TypeRepr.of[Any]
            else if (a =:= TypeRepr.of[Any])
              TypeRepr.of[bt]
            else if (b =:= TypeRepr.of[Any])
              TypeRepr.of[at]
            else
              TypeRepr.of[at with bt]
      out
  }

  def computeCommonBaseClass(using Quotes)(a: quotes.reflect.TypeRepr, b: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect._
    import scala.collection.immutable.ListSet
    if (a =:= TypeRepr.of[Nothing] && b =:= TypeRepr.of[Nothing]) TypeRepr.of[Nothing]
    else if (a =:= TypeRepr.of[Nothing]) b
    else if (b =:= TypeRepr.of[Nothing]) a
    else if (a =:= TypeRepr.of[Any] || b =:= TypeRepr.of[Any]) TypeRepr.of[Any]
    else
      val isectOpt = ListSet(a.widen.baseClasses: _*).intersect(ListSet(b.widen.baseClasses: _*)).headOption
      val isectTypeOpt = isectOpt.map(baseClassSymbol => baseClassSymbol.typeRef)
      isectTypeOpt.getOrElse(TypeRepr.of[Any])

  def topLevelOwner(using Quotes): quotes.reflect.Symbol =
    import quotes.reflect._
    // Need to check the name because for some reason checking symbol flags recurisvely upward gives you
    // a |dotty.tools.dotc.core.CyclicReference error.
    findOwner(Symbol.spliceOwner, sym => sym.name == "macro")

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
