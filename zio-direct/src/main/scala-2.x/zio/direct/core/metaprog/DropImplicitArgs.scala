package zio.direct.core.metaprog

import scala.reflect.macros.blackbox.Context

/**
 * Drop all implicit args of some converting a method:
 *   something.foo(x, y)(implicit foo, bar)
 * to:
 *   something.foo(x, y)(implicit foo, bar)
 *
 * If there are only implicit args e.g.
 *   something.foo(implicit, foo, bar)
 * Then only the thing on which the function is called will remain
 *   something.foo(implicit, foo, bar) // will become:
 *     something
 */
object DropImplicitArgs {
  def of(c: Context)(applyNode: c.universe.Apply) = {
    import c.universe._

    def findParamLists(applyNode: Apply) = {
      val fn = applyNode.fun
      for {
        methodSym <- if (fn.symbol.isMethod) Some(fn.symbol) else None
        firstParamList <- Some(fn.symbol.asMethod.paramLists) // .find(params => params.headOption.exists(_.isParameter))
      } yield firstParamList
    }

    def transformNestedApply(applyNode: Apply, paramListsOpt: Option[List[List[Symbol]]]): Tree = {
      val lastParamListOpt = paramListsOpt.map(_.lastOption).flatten
      val nonImplicitArgs = ImplicitArgs.fromFunctionMarked(c)(applyNode, lastParamListOpt).filter { case (_, stat) => !stat.isImplicit }.map(_._1)

      val newFun =
        applyNode.fun match {
          case innerApply @ Apply(_, _) =>
            transformNestedApply(innerApply, paramListsOpt.map(_.dropRight(1)))
          case other =>
            other
        }

      if (nonImplicitArgs.length > 0)
        Apply(newFun, nonImplicitArgs)
      else {
        newFun
      }
    }

    val functionParams = findParamLists(applyNode)
    transformNestedApply(applyNode, functionParams)
  }
}

private[metaprog] object ImplicitArgs {
  sealed trait ArgType {
    def isImplicit: Boolean
  }
  object ArgType {
    case object Implicit extends ArgType { val isImplicit = true }
    case object Regular extends ArgType { val isImplicit = false }
  }

  // Get the arguments from the unapply if they are not implicit. Since implicit-ness is typically defined
  // on the clause (I think in some rare cases the term itself can be implicit) so if the function-clause
  // of the Apply is implicit then this list will be empty. Otherwise, it will consist of all the arugments.
  def fromFunctionMarked(c: Context)(applyNode: c.universe.Apply, paramList: Option[List[c.universe.Symbol]]) = {
    import c.universe._

    val firstParams = paramList
    val argsRaw = applyNode.args

    // Sometimes we don't know directly from the Term whether it is implicit or not, try to get the
    // arg val-defs and see if they are marked implicit there. All of this needs to be added to the code
    // formatting logic.
    val argsJoined: List[(Tree, Option[Symbol])] =
      firstParams match {
        case Some(parmValDefs) if (parmValDefs.length == argsRaw.length) =>
          argsRaw.zip(parmValDefs.map(Some(_)))
        case _ =>
          argsRaw.map(arg => (arg, None))
      }

    argsJoined.map {
      case (arg, argValDef) => {
        val isValDefImplicit = argValDef.exists(vd => vd.isImplicit)
        (arg, { if (isValDefImplicit) ArgType.Implicit else ArgType.Regular })
      }
    }
  }
}
