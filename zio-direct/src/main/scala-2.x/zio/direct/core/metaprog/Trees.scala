package zio.direct.core.metaprog

import scala.reflect.macros.blackbox.Context

private[direct] object Trees {

  object Transform {
    def apply(c: Context)(tree: c.Tree)(pf: PartialFunction[c.Tree, c.Tree]): c.Tree = {
      import c.universe._
      new Transformer {
        override def transform(tree: Tree) =
          pf.lift(tree).getOrElse(super.transform(tree))
      }.transform(tree)
    }
    def unapply(c: Context)(tree: c.Tree)(pf: PartialFunction[c.Tree, c.Tree]): Option[c.Tree] =
      apply(c)(tree)(pf) match {
        case `tree` => None
        case tree   => Some(tree)
      }
  }

  def traverse(c: Context)(tree: c.Tree)(pf: PartialFunction[c.Tree, Unit]) = {
    import c.universe._
    new Traverser {
      override def traverse(tree: Tree) = {
        // if the partial function matches it will run whatever logic is therin. Otherwise we don't care about the value
        pf.applyOrElse((tree: Tree), (tree: Tree) => ())
        // In either case, proceed further up the tree
        super.traverse(tree)
      }
    }.traverse(tree)
  }
  def exists(c: Context)(tree: c.Tree)(pf: PartialFunction[c.Tree, Boolean]) = {
    var r = false
    traverse(c)(tree) {
      case t if pf.isDefinedAt(t) && !r => r = pf(t)
    }
    r
  }
}
