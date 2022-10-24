package zio.asyncawait.core.metaprog

import scala.quoted._
import zio.asyncawait.core.metaprog.Extractors.Lambda1

object ValDefStatement:
  def unapply(using Quotes)(stmt: quotes.reflect.Statement): Option[(quotes.reflect.Symbol, quotes.reflect.Term)] =
    import quotes.reflect._
    stmt match {
      case valdef @ ValDef(name, tpe, rhsOpt) =>
        val body =
          rhsOpt match {
            // TODO Better site-description in error
            case None      => report.throwError(s"Cannot parse 'val' clause with no '= rhs' (i.e. equals and right hand side) of ${Printer.TreeStructure.show(stmt)}")
            case Some(rhs) => rhs
          }

        Some((valdef.symbol, body))

      case _ => None
    }
end ValDefStatement

object Trees:
  object Transform:
    def apply(using Quotes)(term: quotes.reflect.Term, owner: quotes.reflect.Symbol)(pf: PartialFunction[quotes.reflect.Term, quotes.reflect.Term]) =
      import quotes.reflect._
      (new TreeMap:
        override def transformTerm(tree: Term)(owner: Symbol): Term = {
          pf.lift(tree).getOrElse(super.transformTerm(tree)(owner))
        }
      ).transformTerm(term)(owner)

  def traverse(using Quotes)(tree: quotes.reflect.Tree, owner: quotes.reflect.Symbol)(pf: PartialFunction[quotes.reflect.Tree, Unit]) =
    import quotes.reflect._
    (new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        pf.lift(tree).getOrElse(super.traverseTree(tree)(owner))
      }
    ).traverseTree(tree)(owner)

  def exists(using Quotes)(tree: quotes.reflect.Tree, owner: quotes.reflect.Symbol)(pf: PartialFunction[quotes.reflect.Tree, Boolean]) = {
    import quotes.reflect._
    var r = false
    traverse(using quotes)(tree, owner) {
      case t if pf.isDefinedAt(t) && !r => r = pf(t)
    }
    r
  }

  def replaceIdent(using Quotes)(tree: quotes.reflect.Term)(oldIdentSymbol: quotes.reflect.Symbol, newIdentSymbol: quotes.reflect.Symbol): quotes.reflect.Term = {
    import quotes.reflect._
    (new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term = {
        tree match
          case tree: Ident if (tree.symbol == oldIdentSymbol) =>
            println(s"============+ REPLACEING IDENT: ${oldIdentSymbol} -> ${newIdentSymbol}")
            //val newSym = Symbol.newMethod(owner, arg1.symbol.name, arg1.tpe.widen)
            Ident(newIdentSymbol.termRef)
          case other =>
            super.transformTerm(other)(oldIdentSymbol.owner)
      }
    ).transformTerm(tree)(oldIdentSymbol.owner)
  }
end Trees