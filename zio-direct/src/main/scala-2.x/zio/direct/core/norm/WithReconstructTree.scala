package zio.direct.core.norm

import zio.direct.core.metaprog.WithIR
import zio.direct.core.metaprog.WithPrintIR
import zio.direct.core.util.WithInterpolator
import zio.direct.core.metaprog.Instructions
import zio.direct.core.metaprog.Collect
import zio.direct.core.metaprog.WithZioType
import zio.direct.core.metaprog.Collect.Sequence
import zio.direct.core.metaprog.Collect.Parallel
import scala.annotation.nowarn
import zio.direct.core.metaprog.MacroBase

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait WithReconstructTree extends MacroBase {
  self: WithIR with WithZioType with WithComputeType with WithPrintIR with WithInterpolator =>

  import c.universe._

  implicit class TermNameOptOps(termName: Option[TermName]) {
    private val wildcard = TermName("_")
    def orWildcard = termName.getOrElse(wildcard)
  }

  protected object ReconstructTree {
    def apply(instructions: Instructions) =
      new ReconstructTree(instructions)
  }
  protected class ReconstructTree private (instructions: Instructions) {
    implicit val instructionsInst: Instructions = instructions
    def fromIR(ir: IR) = apply(ir, true)

    // private def computeSymbolType(valSymbol: Option[Symbol], alternativeSource: Term) =
    //   valSymbol match
    //     // TODO Add a verification here that the alternativeSource [a] symbol has has the same type as valSymbol?
    //     case Some(oldSymbol) =>
    //       oldSymbol.termRef.widenTermRefByName.asType
    //     case None =>
    //       alternativeSource.tpe.asType match
    //         case '[ZIO[r, e, a]] => Type.of[a]

    private def compressBlock(accum: List[Tree], block: IR.Block): (List[Tree], Tree) =
      block.tail match {
        case nextBlock: IR.Block =>
          compressBlock(accum :+ block.head, nextBlock)
        case otherMonad =>
          (accum :+ block.head, apply(otherMonad))
      }

    @nowarn("msg=Recursive call used default arguments instead of passing current argument values.")
    private def apply(ir: IR, isTopLevel: Boolean = false): Tree = {
      ir match {
        case IR.Pure(code) => ZioApply.succeed(code)

        // IR.Unsafe is a construct used to modify the internal tree via the WrapUnsafes phase.
        // As such, we don't need to anything special with it during tree-reconstruction.
        case IR.Unsafe(body) =>
          apply(body)

        case IR.FlatMap(monad, valSymbol, body) => {
          val monadExpr = apply(monad)
          val bodyExpr = apply(body)
          q"$monadExpr.flatMap((${toVal(valSymbol.orWildcard)}) => $bodyExpr)"
        }

        case IR.ValDef(origStmt, symbol, assignment, bodyUsingVal) =>
          (assignment, bodyUsingVal) match {
            // E.g: { val x = 123; somethingPure } - If it is a totally pure value, just return the original statement
            // case (_: IR.Pure, _: IR.Pure) => Some(origStmt)
            case (_: IR.Pure, _: IR.Pure) =>
              apply(IR.Pure(origStmt))
            // The following cases are possible:
            // 0. Pure/Pure     - { val x = 123; somethingPure }
            // 1. Pure/Impure   - { val x = 123; somethingMonadic }
            // 2. Impure/Pure   - { val x = succeed(123).run; somethingPure }
            // 3. Impure/Impure - { val x = succeed(123).run; somethingMonadic }
            case (_, pureBody: IR.Pure) =>
              // remove the 1st case by making the assingment monadic (wrap it if needed)
              apply(IR.Map(IR.Monad(apply(assignment)), symbol, pureBody))
            case (_, monadicBody: IR.Monadic) =>
              apply(IR.FlatMap(IR.Monad(apply(assignment)), symbol, monadicBody))
          }

        case IR.Foreach(listIR, listType, elementSymbol, body) =>
          // For something like
          //   (list:Iterable[E]).foreach(e => body)
          // the `sym` is of type E because it is `e`
          // We need to transform it into something like:
          //   ZIO.succeed(list).map { (l:Iterable[E] => ZIO.foreach(l)(body) }
          val monadExpr = apply(listIR)
          val bodyMonad = apply(body)
          // val elementType = elementSymbol.termRef.widenTermRefByName.asType
          val listVar = freshName("list")
          instructions.collect match {
            case Sequence =>
              q"""
                $monadExpr.flatMap((${toVal(listVar)}) =>
                  zio.ZIO.foreach($listVar)(${toVal(elementSymbol)} =>
                    $bodyMonad
                  ).map(_ => ())
                )
              """
            case Parallel =>
              q"""
                $monadExpr.flatMap((${toVal(listVar)}) =>
                  zio.ZIO.foreachPar($listVar)(${toVal(elementSymbol)} =>
                    $bodyMonad
                  ).map(_ => ())
                )
              """
          }

        // Pull out the value from IR.Pure and use it directly in the mapping
        case IR.Map(monad, valSymbol, IR.Pure(body)) =>
          val monadExpr = apply(monad)
          // def symbolType = computeSymbolType(valSymbol, monadExpr)
          q"$monadExpr.map((${toVal(valSymbol.orWildcard)}) => ${body})"

        case IR.Monad(code) =>
          code

        case IR.Fail(error) =>
          error match {
            case IR.Pure(value) =>
              val tpe = ComputeType.fromIR(error).a.widen
              q"zio.ZIO.fail[$tpe](${value})"

            // TODO test the case where error is constructed via an run
            case m: IR.Monadic =>
              val monad = apply(m)
              val a = ComputeType.fromIR(error).a
              q"$monad.flatMap(err => zio.ZIO.fail[$a](err))"
          }

        case block: IR.Block =>
          val (stmts, term) = compressBlock(List(), block)

          // if we are on the top-level we are not inside of any map or flatMap
          // which means that we need to nest any possible exceptions into ZIO.succeed
          // so that they will go into the effect system instead of directly to the outside
          val blockExpr = Block(stmts, term) // .asExprOf[ZIO[?, ?, ?]]
          if (isTopLevel)
            q"${ZioApply.succeed(blockExpr)}.flatten"
          else
            blockExpr

        case value: IR.Match =>
          reconstructMatch(value)

        case value: IR.If =>
          reconstructIf(value)

        case expr @ IR.And(left, right) =>
          (left, right) match {
            case (a: IR.Monadic, b: IR.Monadic) =>
              q"${apply(a)}.flatMap { case true => ${apply(b)}; case false => ${ZioApply.False} }"
            case (a: IR.Monadic, IR.Pure(b)) =>
              q"${apply(a)}.map { case true => $b; case false => false }"
            case (IR.Pure(a), b: IR.Monadic) =>
              /*inside the condition a.asExprOf[Boolean]*/
              q"""
                if ($a) ${apply(b)}
                else ${ZioApply.False}
              """
            // case Pure/Pure is taken care by in the transformer on a higher-level via the PureTree case. Still, handle them just in case
            case (IR.Pure(a), IR.Pure(b)) =>
              // a and b are .asExprOf[Boolean]
              q"zio.ZIO.succeed($a && $b)"
            case _ => report.errorAndAbort(s"Invalid boolean variable combination:\n${PrintIR(expr)}")
          }

        case expr @ IR.Or(left, right) =>
          (left, right) match {
            case (a: IR.Monadic, b: IR.Monadic) =>
              q"${apply(a)}.flatMap { case true => ${ZioApply.True}; case false => ${apply(b)} }"
            case (a: IR.Monadic, IR.Pure(b)) =>
              q"${apply(a)}.map { case true => true; case false => ${b} }"
            case (IR.Pure(a), b: IR.Monadic) =>
              q"""
                if ($a) ${ZioApply.True}
                else ${apply(b)}
              """
            // case Pure/Pure is taken care by in the transformer on a higher-level via the PureTree case. Still, handle them just in case
            case (IR.Pure(a), IR.Pure(b)) =>
              q"zio.ZIO.succeed($a || $b)"
            case _ => report.errorAndAbort(s"Invalid boolean variable combination:\n${PrintIR(expr)}")
          }

        // TODO test with dependencyes and various errors types in both condition and body
        case irWhile @ IR.While(whileCond, whileBody) =>
          // Since the function needs to know the type for the definition (especially because it's recursive!) need to find that out here
          val methOutputComputed = ComputeType.fromIR(irWhile)
          val methOutputTpe = methOutputComputed.toZioType

          // val methodType = MethodType(Nil)(_ => Nil, _ => methOutputTpe)
          // val methSym = Symbol.newMethod(Symbol.spliceOwner, "whileFunc", methodType)

          val whileFunc = TermName(c.freshName("whileFunc"))
          // val newBody = Transform(q"if($cond) { $body; ${c.prefix}.unlift[scala.Unit]($name()) }")
          // Some(q"{ def $name(): $unitMonadType = $newBody; $name() }")

          // This should be something like:
          // if (whileCond) flatMap(whileBody, _, whileFunc()) else ZIO.succeed(())

          // val dd = DefDef(Modifiers(), )

          val newMethodBody =
            IR.If(
              whileCond,
              IR.FlatMap(
                IR.Monad(apply(whileBody)),
                None,
                IR.Monad(q"$whileFunc()")
                // IR.Monad(q"???.asInstanceOf[$methOutputTpe]")
              ),
              IR.Pure(q"()")
            )
          val newMethodBodyExpr = apply(newMethodBody)
          val newMethod = q"def $whileFunc(): $methOutputTpe = $newMethodBodyExpr"
          apply(IR.Block(newMethod, IR.Monad(q"$whileFunc()")))

        // val newBody = Transform(q"if($cond) { $body; ${c.prefix}.unlift[scala.Unit]($name()) }")
        // Some(q"{ def $name(): $unitMonadType = $newBody; $name() }")

        case tryIR @ IR.Try(tryBlock, cases, _, finallyBlock) =>
          ComputeType.fromIR(tryBlock)
          val newCaseDefs = reconstructCaseDefs(cases)
          val tryTerm = apply(tryBlock)
          val (r, e, a) = ComputeType.fromIR(tryIR).asTypeTuple

          // need to cast to .asInstanceOf[zio.ZIO[$r, _, $a]] erasing the type _ since it could be
          // `Nothing` and then ZIO returns the following error:
          // "This error handling operation assumes your effect can fail. However, your effect has Nothing for the error type"
          val monadExpr = q"{ $tryTerm.asInstanceOf[zio.ZIO[$r, _, $a]].catchSome { case ..$newCaseDefs } }"

          // .asInstanceOf[zio.ZIO[$r, _, $a]]

          finallyBlock match {
            case Some(ir) =>
              val finallyExpr = apply(ir)
              q"$monadExpr.ensuring(zio.direct.core.util.ZioUtil.wrapWithThrowable($finallyExpr).orDie)"

            case None =>
              monadExpr
          }

        case value: IR.Parallel =>
          reconstructParallel(value)
      }
    }

    def reconstructMatch(value: IR.Match) = {
      val IR.Match(scrutinee, caseDefs) = value
      scrutinee match {
        case value: IR.Monadic =>
          val monadExpr = apply(value)
          // use the symbol instead of the monad as the scrutinee because the monad needs to be flatmapped first
          // (Note don't think you need to change ownership of caseDef.rhs to the new symbol but possible.)
          val matchSymbol = freshName("matchVar")
          val newCaseDefs = reconstructCaseDefs(caseDefs)
          // Possible exploration: if the content of the match is pure we lifted it into a monad. If we want to optimize we
          // change the IR.CaseDef.rhs to be IR.Pure as well as IR.Monadic and handle both cases
          val newMatch = Match(q"$matchSymbol", newCaseDefs)
          // We can synthesize the monadExpr.flatMap call from the monad at this point but I would rather pass it to the FlatMap case to take care of
          apply(IR.FlatMap(IR.Monad(monadExpr), matchSymbol, IR.Monad(newMatch)))
        case IR.Pure(termValue) =>
          val newCaseDefs = reconstructCaseDefs(caseDefs)
          val newMatch = Match(termValue, newCaseDefs)
          // recall that the expressions in the case defs need all be ZIO instances (i.e. monadic) we we can
          // treat the whole thing as a ZIO (i.e. monadic) expression
          newMatch // .asExprOf[ZIO[?, ?, ?]]
      }
    }

    private def reconstructCaseDefs(caseDefs: List[IR.Match.CaseDef]) =
      caseDefs.map { caseDef =>
        val bodyExpr = apply(caseDef.rhs)
        CaseDef(caseDef.pattern, caseDef.guard.getOrElse(EmptyTree), bodyExpr)
      }

    def reconstructIf(value: IR.If) = {
      val IR.If(cond, ifTrue, ifFalse) = value
      sealed trait ConditionState
      object ConditionState {
        case class BothPure(ifTrue: Tree, ifFalse: Tree) extends ConditionState
        case class BothMonadic(ifTrue: IR.Monadic, ifFalse: IR.Monadic) extends ConditionState
      }

      val conditionState =
        (ifTrue, ifFalse) match {
          case (IR.Pure(a), IR.Pure(b))       => ConditionState.BothPure(a, b)
          case (a: IR.Pure, b: IR.Monadic)    => ConditionState.BothMonadic(IR.Monad(apply(a)), b)
          case (a: IR.Monadic, b: IR.Pure)    => ConditionState.BothMonadic(a, IR.Monad(apply(b)))
          case (a: IR.Monadic, b: IR.Monadic) => ConditionState.BothMonadic(a, b)
        }

      cond match {
        case m: IR.Monadic => {
          val sym = freshName("ifVar")
          conditionState match {
            // For example: if(run(something)) run(foo) else run(bar)
            // => something.map(ifVar => (foo, bar) /*replace-to-ifVar*/)
            // Note that in this case we embed foo, bar into the if-statement. They are ZIO-values which is why we need a flatMap
            case ConditionState.BothMonadic(ifTrue, ifFalse) =>
              val ifTrueTerm = apply(ifTrue)
              val ifFalseTerm = apply(ifFalse)
              apply(IR.FlatMap(m, sym, IR.Monad(If(q"$sym", ifTrueTerm, ifFalseTerm))))
            // For example: if(run(something)) "foo" else "bar"
            case ConditionState.BothPure(ifTrue, ifFalse) =>
              apply(IR.Map(m, sym, IR.Pure(If(q"$sym", ifTrue, ifFalse))))
          }
        }
        case IR.Pure(value) => {
          conditionState match {
            case ConditionState.BothMonadic(ifTrue, ifFalse) =>
              val ifTrueTerm = apply(ifTrue)
              val ifFalseTerm = apply(ifFalse)
              If(value, ifTrueTerm, ifFalseTerm) // .asExprOf[ZIO[?, ?, ?]]
            case ConditionState.BothPure(ifTrue, ifFalse) =>
              val ifStatement = If(value, ifTrue, ifFalse)
              apply(IR.Pure(ifStatement))
          }
        }
      }
    }

    def reconstructParallel(value: IR.Parallel): Tree = { // Expr[ZIO[?, ?, ?]]
      val IR.Parallel(_, unlifts, newTree) = value
      unlifts.toList match {
        case List() =>
          newTree match {
            case IR.Pure(newTree)  => ZioApply.succeed(newTree)
            case IR.Monad(newTree) => newTree // .asExprOf[ZIO[?, ?, ?]]
          }

        /*
        For a expression (in a single block-line) that has one run in the middle of things e.g.
        { run(foo) + bar }
        Needs to turn into something like:
        { run(foo).map(fooVal => fooVal + bar) }
        When thinking about types, it looks something like:
        { (run(foo:Task[t]):t + bar):r }
        { run(foo:t):Task[t].map[r](fooVal:t => (fooVal + bar):r) }
         */
        case List((monad, oldSymbol, tpe)) => {
          // wrap a body of code that pointed to some identifier whose symbol is prevValSymbol
          // def wrapWithLambda[T: Type, R: Type](body: Term, prevValSymbol: Symbol) = {
          //   val mtpe = MethodType(List("sm"))(_ => List(TypeRepr.of[T]), _ => TypeRepr.of[R])
          //   Lambda(
          //     Symbol.spliceOwner,
          //     mtpe,
          //     {
          //       case (methSym, List(sm: Term)) => replaceSymbolIn(body)(prevValSymbol, sm).changeOwner(methSym)
          //       case _                         => report.errorAndAbort("Not a possible state")
          //     }
          //   )
          // }

          val monadExpr = apply(monad)
          newTree match {
            case IR.Pure(newTree) =>
              // val lam = wrapWithLambda[t, r](newTree, oldSymbol)
              apply(IR.Monad(q"${monadExpr}.map((${toVal(oldSymbol)}) => $newTree)"))
            case IR.Monad(newTree) =>
              // val lam = wrapWithLambda[t, ZIO[x1, y1, r]](newTree, oldSymbol)
              // could also work like this?
              // apply(IR.Monad('{ ${monadExpr.asExprOf[ZIO[Any, Nothing, t]]}.flatMap[Any, Nothing, r](${lam.asExprOf[t => ZIO[Any, Nothing, r]]}) }.asTerm))
              // apply(IR.Monad('{ ${ monadExpr.asExprOf[ZIO[?, ?, t]] }.flatMap(${ lam.asExprOf[t => ZIO[x1, y1, r]] }) }.asTerm))
              apply(IR.Monad(q"${monadExpr}.flatMap((${toVal(oldSymbol)}) => $newTree)"))
          }

        }
        case unlifts =>
          val unliftTriples =
            unlifts.map {
              case (monad, monadSymbol, tpe) => {
                val monadExpr = apply(monad)
                (monadExpr, monadSymbol, tpe.widen)
              }
            }
          val (terms, names, tpes) = unliftTriples.unzip3
          val collect =
            instructions.collect match {
              case Collect.Sequence =>
                q"zio.ZIO.collectAll(List(..$terms))"
              case Collect.Parallel =>
                q"zio.ZIO.collectAllPar(List(..$terms))"
            }

          val list = freshName("list")
          val iterator = freshName("iterator")

          def makeVariables = // iterator: Expr[Iterator[?]]
            unliftTriples.map { case (monad, symbol, tpe) =>
              // ValDef(symbol, Some('{ $iterator.next().asInstanceOf[t] }.asTerm))
              q"val $symbol = $iterator.next().asInstanceOf[$tpe]"
            }

          val output =
            newTree match {
              case IR.Pure(newTree) =>
                q"""
                  $collect.map(${toVal(list)} => {
                    val $iterator = $list.iterator
                    ..$makeVariables
                    $newTree
                  })
                """
              case IR.Monad(newTree) =>
                q"""
                  $collect.flatMap(${toVal(list)} => {
                    val $iterator = $list.iterator
                    ..$makeVariables
                    $newTree
                  })
                """
            }

          apply(IR.Monad(output))
      }
    }
  }

}
