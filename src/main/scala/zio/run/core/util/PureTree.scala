package zio.run.core.util

import scala.quoted._
import zio.run.core.metaprog.Trees
import zio.run.core.metaprog.Extractors._
import zio.run.await

object PureTree:
  object All:
    def unapply(using Quotes)(tree: List[quotes.reflect.Statement]): Boolean =
      tree.forall(PureTree.unapply(_).isDefined)

  def unapply(using Quotes)(tree: quotes.reflect.Tree): Option[quotes.reflect.Tree] =
    import quotes.reflect._
    Trees.exists(tree, Symbol.spliceOwner) {
      case Seal('{ await[r, e, a]($v) }) => true
    } match {
      case true  => None
      case false => Some(tree)
    }
