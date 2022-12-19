package zio.direct.core.metaprog.compat

import zio.direct.core.metaprog.MacroBase

trait WithAllowedCompat extends MacroBase {
  import c.universe._

  object NamedArgCompat {
    def unapply(tree: Tree): Option[Tree] =
      tree match {
        case NamedArg(_, _) => Some(tree)
        case _              => None
      }
  }

  object AssignCompat {
    def unapply(tree: Tree): Option[Tree] =
      tree match {
        case Assign(_, _) => Some(tree)
        case _            => None
      }
  }
}
