package zio.direct.core.metaprog

import scala.quoted._
import zio.direct.core.util.Format

object Trees:
  object TransformTree:
    def apply(using Quotes)(rawTree: quotes.reflect.Tree, owner: quotes.reflect.Symbol)(pf: PartialFunction[quotes.reflect.Tree, quotes.reflect.Tree]) =
      import quotes.reflect._
      var counter = 0
      (new TreeMap:
        override def transformStatement(stmt: Statement)(owner: Symbol): Statement = {
          val lifted =
            pf.lift(stmt) match {
              case Some(term: Statement) => Some(term)
              case Some(other)           => report.errorAndAbort(s"Transformed the term ${Format.Tree(other)} into something that is not a Statement: ${Format.Tree(other)}")
              case None                  => None
            }
          lifted.getOrElse(super.transformStatement(stmt)(owner))
        }
        override def transformTerm(stmt: Term)(owner: Symbol): Term = {
          val lifted =
            pf.lift(stmt) match {
              case Some(term: Term) => Some(term)
              case Some(other)      => report.errorAndAbort(s"Transformed the term ${Format.Tree(other)} into something that is not a Term: ${Format.Tree(other)}")
              case None             => None
            }
          lifted.getOrElse(super.transformTerm(stmt)(owner))
        }
      ).transformTree(rawTree)(owner).asInstanceOf[Tree]

  object Transform:
    def apply(using Quotes)(term: quotes.reflect.Term, owner: quotes.reflect.Symbol)(pf: PartialFunction[quotes.reflect.Term, quotes.reflect.Term]) =
      import quotes.reflect._
      (new TreeMap:
        override def transformTerm(tree: Term)(owner: Symbol): Term = {
          // In this case we need to return an actual value for each construct in the tree so that means
          // that when the partial function that matches a term executes after the => arrow, it needs to actually
          // define which nodes to go into next. This is no the case for just `traverse` where we keep going down the tree
          // no mater what happens since each partial-function invocation doesn't need to actually return anything.
          pf.lift(tree).getOrElse(super.transformTerm(tree)(owner))
        }
      ).transformTerm(term)(owner)

  def traverse(using Quotes)(tree: quotes.reflect.Tree, owner: quotes.reflect.Symbol)(pf: PartialFunction[quotes.reflect.Tree, Unit]) =
    import quotes.reflect._
    (new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        // if the partial function matches it will run whatever logic is therin. Otherwise we don't care about the value
        pf.applyOrElse(tree, tree => ())
        // In either case, proceed further up the tree
        super.traverseTree(tree)(owner)
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

  def replaceIdent(using Quotes)(tree: quotes.reflect.Term)(oldIdentSymbol: quotes.reflect.Symbol, newTerm: quotes.reflect.Term): quotes.reflect.Term = {
    import quotes.reflect._
    (new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term = {
        tree match
          case tree: Ident if (tree.symbol == oldIdentSymbol) =>
            newTerm
          case other =>
            super.transformTerm(other)(oldIdentSymbol.owner)
      }
    ).transformTerm(tree)(oldIdentSymbol.owner)
  }

  def replaceIdents(using Quotes)(of: quotes.reflect.Term, treeOwner: quotes.reflect.Symbol)(replacements: (quotes.reflect.Symbol, quotes.reflect.Term)*): quotes.reflect.Term = {
    val mappings = replacements.toMap
    import quotes.reflect._
    (new TreeMap:
      override def transformTerm(term: Term)(owner: Symbol): Term = {
        term match
          case id: Ident if (mappings.contains(id.symbol)) =>
            mappings(id.symbol)
          case other =>
            super.transformTerm(other)(owner)
      }
    ).transformTerm(of)(treeOwner)
  }
end Trees
