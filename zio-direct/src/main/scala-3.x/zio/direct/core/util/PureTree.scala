package zio.direct.core.util

import scala.quoted._
import zio.direct.core.metaprog.Trees
import zio.direct.core.metaprog.Extractors._
import zio.direct.unsafe

object PureTree:
  object All:
    def unapply(using Quotes)(tree: List[quotes.reflect.Statement]): Boolean =
      tree.forall(PureTree.unapply(_).isDefined)

  def unapply(using Quotes)(tree: quotes.reflect.Tree): Option[quotes.reflect.Tree] =
    import quotes.reflect._
    // If the below terms exist always treat the tree as something that needs to be introspected
    Trees.exists(tree, Symbol.spliceOwner) {
      case AnyRunCall(_)             => true
      case Seal('{ unsafe($value) }) => true
      case Try(_, _, _)              => true
      case Seal('{ throw $e })       => true
      case _                         => false
    } match {
      case true  => None
      case false => Some(tree)
    }
