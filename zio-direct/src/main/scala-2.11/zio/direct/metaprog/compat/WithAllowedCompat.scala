package zio.direct.core.metaprog.compat

import zio.direct.core.metaprog.MacroBase

trait WithAllowedCompat extends MacroBase {
  import c.universe._

  object NamedArgCompat {
    def unapply(tree: Tree): Option[Tree] =
      tree match {
        /*
        // When you make a function
        def myFun(foo: String = "foo", bar: String = "bar") = foo + bar
        // .. and use a named arg like so
        myFun(bar = "blah")

        // Most of the time, Scala 2.12 will make a convoluted construct
        // to look like a NamedArg that looks like this:
        Block(
          List(
            ValDef(Mods(ARTIFACT), fakeBarArg Literal("blah")),
            ValDef(Mods(ARTIFACT),"fakeDefaultFooArg")
          ),
          Apply(
            Select(fun), List(Ident(fakeDefaultFoo), Ident(fakeBarArg)))
          )
        )

        Now because these vals are already synthetic, zio-direct already allows them
        in WithAllowed.scala. In rare cases AssignOrNamedArg is actually used and
        in that case, we check to see that the lhs (presumeably the varaible `x`
        in a `fun(x=something)` construct) is a paramter.
         */
        case v @ AssignOrNamedArg(lhs, _) if (lhs.symbol.isParameter) => Some(tree)
        case _                                                        => None
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
