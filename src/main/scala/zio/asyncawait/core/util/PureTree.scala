package zio.asyncawait.core.util

import scala.quoted._
import zio.asyncawait.core.metaprog.Trees
import zio.asyncawait.core.metaprog.Extractors._
import zio.asyncawait.await

object PureTree:
  object All:
    def unapply(using Quotes)(tree: List[quotes.reflect.Statement]): Boolean =
      tree.forall(PureTree.unapply(_).isDefined)

  def unapply(using Quotes)(tree: quotes.reflect.Tree): Option[quotes.reflect.Tree] =
    import quotes.reflect._
    Trees.exists(tree, Symbol.spliceOwner) {
      case Seal('{ await[r, e, a]($v) }) => true
    } match {
      case true => None
      case false => Some(tree)
    }