package zio.direct.core.norm

import zio.direct.core.metaprog.WithIR
import scala.quoted._
import zio.direct.core.metaprog.Embedder._
import zio.ZIO
import zio.direct.core.metaprog.WithPrintIR
import zio.Chunk
import zio.direct.core.util.ComputeTotalZioType
import zio.direct.core.util.Format
import zio.direct.core.util.WithInterpolator
import zio.Exit.Success
import zio.Exit.Failure
import zio.direct.core.metaprog.Instructions
import zio.direct.core.metaprog.Collect
import zio.direct.core.util.ZioUtil

trait WithReconstructTree {
  self: WithIR with WithComputeType with WithPrintIR with WithInterpolator =>

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  protected object ReconstructTree {
    def apply(instructions: Instructions) =
      new ReconstructTree(instructions)
  }
  protected class ReconstructTree private (instructions: Instructions) {
    implicit val instructionsInst: Instructions = instructions
    def fromIR(ir: IR) = apply(ir, true)

    private def computeSymbolType(valSymbol: Option[Symbol], alternativeSource: Term) =
      valSymbol match
        case Some(oldSymbol) =>
          oldSymbol.termRef.widenTermRefByName.asType
        case None =>
          alternativeSource.tpe.asType match
            case '[ZIO[r, e, a]] => Type.of[a]

    private def compressBlock(accum: List[Statement] = List(), block: IR.Block): (List[Statement], Term) =
      block.tail match
        case nextBlock: IR.Block =>
          compressBlock(accum :+ block.head, nextBlock)
        case otherMonad =>
          (accum :+ block.head, apply(otherMonad).asTerm)

    private def apply(ir: IR, isTopLevel: Boolean = false): Expr[ZIO[?, ?, ?]] = {
      ir match
        case IR.Pure(code) => ZioApply.succeed(code)

        // IR.Unsafe is a construct used to modify the internal tree via the WrapUnsafes phase.
        // As such, we don't need to anything special with it during tree-reconstruction.
        case IR.Unsafe(body) =>
          apply(body)

        case IR.FlatMap(monad, valSymbol, body) => {
          val monadExpr = apply(monad)
          val bodyExpr = apply(body)
          def symbolType = computeSymbolType(valSymbol, monadExpr.asTerm)
          // Symbol type needs to be the same as the A-parameter of the ZIO, if not it's an error
          // should possibly introduce an asserition for that
          // Also:
          // TODO synthesize + eta-expand the lambda manually so it's ame is based on the previous symbol name

          symbolType match
            case '[t] =>
              '{
                $monadExpr.asInstanceOf[ZIO[?, ?, t]].flatMap((v: t) =>
                  ${
                    replaceSymbolInBodyMaybe(using macroQuotes)(bodyExpr.asTerm)(valSymbol, ('v).asTerm).asExprOf[ZIO[?, ?, ?]]
                  }
                )
              }
        }

        // Pull out the value from IR.Pure and use it directly in the mapping
        case IR.Map(monad, valSymbol, IR.Pure(body)) =>
          val monadExpr = apply(monad)
          def symbolType = computeSymbolType(valSymbol, monadExpr.asTerm)
          symbolType match
            // TODO check that 'a' is the same as 't' here?
            case '[t] =>
              '{
                $monadExpr.asInstanceOf[ZIO[?, ?, t]].map((v: t) =>
                  ${ replaceSymbolInBodyMaybe(using macroQuotes)(body)(valSymbol, ('v).asTerm).asExpr }
                )
              } // .asInstanceOf[ZIO[?, ?, ?]] // since the body has it's own term not specifying that here
        // TODO any ownership changes needed in the body?

        case IR.Monad(code) => code.asExprOf[ZIO[?, ?, ?]]

        case IR.Fail(error) =>
          error match {
            case IR.Pure(value) =>
              ComputeType.fromIR(error).a.widen.asType match
                case '[a] =>
                  '{ ZIO.fail[a](${ value.asExpr }.asInstanceOf[a]) }
            // TODO test the case where error is constructed via an await
            case m: IR.Monadic =>
              ComputeType.fromIR(error).asTypeTuple match
                case ('[r], '[e], '[a]) =>
                  val monad = apply(m)
                  monad.asTerm.tpe.asType match
                    case '[t] =>
                      '{ ${ monad }.asInstanceOf[ZIO[r, e, a]].flatMap(err => ZIO.fail(err)) }
          }

        case block: IR.Block =>
          val (stmts, term) = compressBlock(List(), block)
          // if we are on the top-level we are not inside of any map or flatMap
          // which means that we need to nest any possible exceptions into ZIO.succeed
          // so that they will go into the effect system instead of directly to the outside
          val blockExpr = Block(stmts, term).asExprOf[ZIO[?, ?, ?]]
          if (isTopLevel)
            '{ ZIO.succeed($blockExpr).flatten }
          else
            blockExpr

        case value: IR.Match =>
          reconstructMatch(value)

        case value: IR.If =>
          reconstructIf(value)

        case expr @ IR.And(left, right) =>
          (left, right) match {
            case (a: IR.Monadic, b: IR.Monadic) =>
              '{ ${ apply(a) }.flatMap { case true => ${ apply(b) }; case false => ${ ZioApply.False } } }
            case (a: IR.Monadic, IR.Pure(b)) =>
              '{ ${ apply(a) }.map { case true => ${ b.asExpr }; case false => ${ ZioApply.False } } }
            case (IR.Pure(a), b: IR.Monadic) =>
              '{
                if (${ a.asExprOf[Boolean] }) ${ apply(b) }
                else ${ ZioApply.False }
              }
            // case Pure/Pure is taken care by in the transformer on a higher-level via the PureTree case. Still, handle them just in case
            case (IR.Pure(a), IR.Pure(b)) =>
              '{ ZIO.succeed(${ a.asExprOf[Boolean] } && ${ b.asExprOf[Boolean] }) }
            case _ => report.errorAndAbort(s"Invalid boolean variable combination:\n${PrintIR(expr)}")
          }

        case expr @ IR.Or(left, right) =>
          (left, right) match {
            case (a: IR.Monadic, b: IR.Monadic) =>
              '{ ${ apply(a) }.flatMap { case true => ${ ZioApply.True }; case false => ${ apply(b) } } }
            case (a: IR.Monadic, IR.Pure(b)) =>
              '{ ${ apply(a) }.map { case true => ${ ZioApply.True }; case false => ${ b.asExpr } } }
            case (IR.Pure(a), b: IR.Monadic) =>
              '{
                if (${ a.asExprOf[Boolean] }) ${ ZioApply.True }
                else ${ apply(b) }
              }
            // case Pure/Pure is taken care by in the transformer on a higher-level via the PureTree case. Still, handle them just in case
            case (IR.Pure(a), IR.Pure(b)) =>
              '{ ZIO.succeed(${ a.asExprOf[Boolean] } || ${ b.asExprOf[Boolean] }) }
            case _ => report.errorAndAbort(s"Invalid boolean variable combination:\n${PrintIR(expr)}")
          }

        // TODO test with dependencyes and various errors types in both condition and body
        case irWhile @ IR.While(whileCond, whileBody) =>
          // Since the function needs to know the type for the definition (especially because it's recursive!) need to find that out here
          val methOutputComputed = ComputeType.fromIR(irWhile)
          val methOutputTpe = methOutputComputed.toZioType

          val methodType = MethodType(Nil)(_ => Nil, _ => methOutputTpe)
          val methSym = Symbol.newMethod(Symbol.spliceOwner, "whileFunc", methodType)

          val newMethodBody =
            IR.If(
              whileCond,
              IR.FlatMap(
                IR.Monad(apply(whileBody).asTerm), // apply().asTerm.changeOwner(methSym),
                None,
                IR.Monad(Apply(Ref(methSym), Nil))
              ),
              IR.Pure('{ () }.asTerm)
            )
          val newMethodBodyExpr =
            methOutputComputed.asTypeTuple match
              case ('[r], '[e], '[a]) =>
                '{ ${ apply(newMethodBody) }.asInstanceOf[ZIO[r, e, a]] }

          val newMethod = DefDef(methSym, sm => Some(newMethodBodyExpr.asTerm))
          // Block(List(newMethod), Apply(Ref(methSym), Nil)).asExprOf[ZIO[?, ?, ?]]
          apply(IR.Block(newMethod, IR.Monad(Apply(Ref(methSym), Nil))))

        case tryIR @ IR.Try(tryBlock, cases, _, finallyBlock) =>
          ComputeType.fromIR(tryBlock).asTypeTuple match
            case ('[rr], '[er], '[ar]) =>
              val newCaseDefs = reconstructCaseDefs(cases)
              val resultType = ComputeType.fromIR(tryIR).toZioType
              val tryTerm = apply(tryBlock)
              (tryTerm.asTerm.tpe.asType, resultType.asType) match
                case ('[ZIO[r0, e0, a0]], '[ZIO[r, e, b]]) => {
                  // A normal lambda looks something like:
                  //   Block(List(
                  //     DefDef(newMethodSymbol, terms:List[List[Tree]] => Option(body))
                  //     Closure(Ref(newMethodSymbol), None)
                  //   ))
                  // A PartialFunction lambda looks looks something like:
                  //   Block(List(
                  //     DefDef(newMethodSymbol, terms:List[List[Tree]] => Option(body))
                  //     Closure(Ref(newMethodSymbol), TypeRepr.of[PartialFunction[input, output]])
                  //   ))
                  // So basically the only difference is in the typing of the closure

                  // Make the new symbol and method types. (Note the `e` parameter extracted from the ZIO above)
                  // Should look something like:
                  //   def tryLamParam(tryLam: e0): ZIO[r, e, a] = ...
                  // Note:
                  //   The `e` type can change because you can specify a ZIO in the response to the try
                  //   e.g: (x:ZIO[Any, Throwable, A]).catchSome { case io: IOException => y:ZIO[Any, OtherExcpetion, A] }
                  val methodType = MethodType(List("tryLamParam"))(_ => List(TypeRepr.of[er]), _ => TypeRepr.of[ZIO[r, e, b]])
                  val methSym = Symbol.newMethod(Symbol.spliceOwner, "tryLam", methodType)

                  // Now we actually make the method with the body:
                  //   def tryLamParam(tryLam: e) = { case ...exception => ... }
                  val method = DefDef(methSym, sm => Some(Match(sm(0)(0).asInstanceOf[Term], newCaseDefs.map(_.changeOwner(methSym)))))
                  // NOTE: Be sure that error here is the same one as used to define tryLamParam. Otherwise, AbstractMethodError errors
                  // saying that .isDefinedAt is abstract will happen.
                  val pfTree = TypeRepr.of[PartialFunction[er, ZIO[r, e, b]]]

                  // Assemble the peices together into a closure
                  val closure = Closure(Ref(methSym), Some(pfTree))
                  val functionBlock = '{ ${ Block(List(method), closure).asExpr }.asInstanceOf[PartialFunction[er, ZIO[r, e, b]]] }
                  val monadExpr = '{ ${ tryTerm }.asInstanceOf[ZIO[rr, er, ar]].catchSome { ${ functionBlock } } }

                  finallyBlock match {
                    case Some(ir) =>
                      val finallyExpr = apply(ir)
                      '{ $monadExpr.ensuring(ZioUtil.wrapWithThrowable($finallyExpr).orDie) }

                    case None =>
                      monadExpr
                  }
                }

        case value: IR.Parallel =>
          reconstructParallel(value)
    }

    def reconstructMatch(value: IR.Match) =
      val IR.Match(scrutinee, caseDefs) = value
      scrutinee match
        case value: IR.Monadic =>
          val monadExpr = apply(value)
          // use the symbol instead of the monad as the scrutinee because the monad needs to be flatmapped first
          // (Note don't think you need to change ownership of caseDef.rhs to the new symbol but possible.)
          val matchSymbol = Symbol.newVal(Symbol.spliceOwner, "matchVar", monadExpr.asTerm.tpe, Flags.EmptyFlags, Symbol.noSymbol)
          val newCaseDefs = reconstructCaseDefs(caseDefs)
          // Possible exploration: if the content of the match is pure we lifted it into a monad. If we want to optimize we
          // change the IR.CaseDef.rhs to be IR.Pure as well as IR.Monadic and handle both cases
          val newMatch = Match(Ref(matchSymbol), newCaseDefs)
          // We can synthesize the monadExpr.flatMap call from the monad at this point but I would rather pass it to the FlatMap case to take care of
          apply(IR.FlatMap(IR.Monad(monadExpr.asTerm), matchSymbol, IR.Monad(newMatch)))
        case IR.Pure(termValue) =>
          val newCaseDefs = reconstructCaseDefs(caseDefs)
          val newMatch = Match(termValue, newCaseDefs)
          // recall that the expressions in the case defs need all be ZIO instances (i.e. monadic) we we can
          // treat the whole thing as a ZIO (i.e. monadic) expression
          newMatch.asExprOf[ZIO[?, ?, ?]]
    end reconstructMatch

    private def reconstructCaseDefs(caseDefs: List[IR.Match.CaseDef]) =
      caseDefs.map { caseDef =>
        val bodyExpr = apply(caseDef.rhs)
        CaseDef(caseDef.pattern, caseDef.guard, bodyExpr.asTerm)
      }

    def reconstructIf(value: IR.If) =
      val IR.If(cond, ifTrue, ifFalse) = value
      enum ConditionState:
        case BothPure(ifTrue: Term, ifFalse: Term)
        case BothMonadic(ifTrue: IR.Monadic, ifFalse: IR.Monadic)

      val conditionState =
        (ifTrue, ifFalse) match {
          case (IR.Pure(a), IR.Pure(b))       => ConditionState.BothPure(a, b)
          case (a: IR.Pure, b: IR.Monadic)    => ConditionState.BothMonadic(IR.Monad(apply(a).asTerm), b)
          case (a: IR.Monadic, b: IR.Pure)    => ConditionState.BothMonadic(a, IR.Monad(apply(b).asTerm))
          case (a: IR.Monadic, b: IR.Monadic) => ConditionState.BothMonadic(a, b)
        }

      cond match {
        case m: IR.Monadic => {
          val sym = Symbol.newVal(Symbol.spliceOwner, "ifVar", TypeRepr.of[Boolean], Flags.EmptyFlags, Symbol.noSymbol)
          conditionState match {
            // For example: if(run(something)) run(foo) else run(bar)
            // => something.map(ifVar => (foo, bar) /*replace-to-ifVar*/)
            // Note that in this case we embed foo, bar into the if-statement. They are ZIO-values which is why we need a flatMap
            case ConditionState.BothMonadic(ifTrue, ifFalse) =>
              val ifTrueTerm = apply(ifTrue).asTerm
              val ifFalseTerm = apply(ifFalse).asTerm
              apply(IR.FlatMap(m, sym, IR.Monad(If(Ref(sym), ifTrueTerm, ifFalseTerm))))
            // For example: if(run(something)) "foo" else "bar"
            case ConditionState.BothPure(ifTrue, ifFalse) =>
              apply(IR.Map(m, sym, IR.Pure(If(Ref(sym), ifTrue, ifFalse))))
          }
        }
        case IR.Pure(value) => {
          conditionState match {
            case ConditionState.BothMonadic(ifTrue, ifFalse) =>
              val ifTrueTerm = apply(ifTrue).asTerm
              val ifFalseTerm = apply(ifFalse).asTerm
              If(value, ifTrueTerm, ifFalseTerm).asExprOf[ZIO[?, ?, ?]]
            case ConditionState.BothPure(ifTrue, ifFalse) =>
              val ifStatement = If(value, ifTrue, ifFalse)
              apply(IR.Pure(ifStatement))
          }
        }
      }
    end reconstructIf

    def reconstructParallel(value: IR.Parallel): Expr[ZIO[?, ?, ?]] =
      val IR.Parallel(unlifts, newTree) = value
      unlifts.toList match {
        case List() =>
          newTree match
            case IR.Pure(newTree)  => '{ ZIO.succeed(${ newTree.asExpr }) }
            case IR.Monad(newTree) => newTree.asExprOf[ZIO[?, ?, ?]]

        /*
        For a expression (in a single block-line) that has one await in the middle of things e.g.
        { run(foo) + bar }
        Needs to turn into something like:
        { run(foo).map(fooVal => fooVal + bar) }
        When thinking about types, it looks something like:
        { (run(foo:Task[t]):t + bar):r }
        { run(foo:t):Task[t].map[r](fooVal:t => (fooVal + bar):r) }
         */
        case List((monad, oldSymbol)) =>
          // wrap a body of code that pointed to some identifier whose symbol is prevValSymbol
          def wrapWithLambda[T: Type, R: Type](body: Term, prevValSymbol: Symbol) = {
            val mtpe = MethodType(List("sm"))(_ => List(TypeRepr.of[T]), _ => TypeRepr.of[R])
            Lambda(
              Symbol.spliceOwner,
              mtpe,
              {
                case (methSym, List(sm: Term)) => replaceSymbolIn(body)(prevValSymbol, sm).changeOwner(methSym)
                case _                         => report.errorAndAbort("Not a possible state")
              }
            )
          }

          val monadExpr = apply(monad)
          (monadExpr.asTerm.tpe.asType) match
            case '[ZIO[x, y, t]] =>
              newTree match
                case IR.Pure(newTree) =>
                  newTree.tpe.asType match
                    case '[r] =>
                      val lam = wrapWithLambda[t, r](newTree, oldSymbol)
                      // TODO what if we do not directly specify 'r' here? what about the flatMap case?
                      apply(IR.Monad('{ ${ monadExpr.asExprOf[ZIO[?, ?, t]] }.map[r](${ lam.asExprOf[t => r] }) }.asTerm))
                case IR.Monad(newTree) =>
                  newTree.tpe.asType match
                    case '[ZIO[x1, y1, r]] =>
                      // println(s"----------- Computed type FlatMap: ${Format.TypeOf[ZIO[x1, y1, r]]}")
                      val lam = wrapWithLambda[t, ZIO[x1, y1, r]](newTree, oldSymbol)
                      // back here
                      // could also work like this?
                      // apply(IR.Monad('{ ${monadExpr.asExprOf[ZIO[Any, Nothing, t]]}.flatMap[Any, Nothing, r](${lam.asExprOf[t => ZIO[Any, Nothing, r]]}) }.asTerm))
                      apply(IR.Monad('{ ${ monadExpr.asExprOf[ZIO[?, ?, t]] }.flatMap(${ lam.asExprOf[t => ZIO[x1, y1, r]] }) }.asTerm))

        case unlifts =>
          val unliftTriples =
            unlifts.map((monad, monadSymbol) => {
              val monadExpr = apply(monad)
              val tpe = monadSymbol.termRef.widenTermRefByName
              (monadExpr.asTerm, monadSymbol, tpe)
            })
          val (terms, names, types) = unliftTriples.unzip3
          val termsExpr = Expr.ofList(terms.map(_.asExprOf[ZIO[?, ?, ?]]))
          val collect =
            instructions.collect match
              case Collect.Sequence =>
                '{ ZIO.collectAll(Chunk.from($termsExpr)) }
              case Collect.Parallel =>
                '{ ZIO.collectAllPar(Chunk.from($termsExpr)) }

          def makeVariables(iterator: Expr[Iterator[?]]) =
            unliftTriples.map((monad, symbol, tpe) =>
              tpe.asType match {
                case '[t] =>
                  ValDef(symbol, Some('{ $iterator.next().asInstanceOf[t] }.asTerm))
              }
            )

          val output =
            ComputeType.fromIR(newTree).transformA(_.widen).asTypeTuple match
              case ('[r], '[e], '[t]) =>
                newTree match
                  case IR.Pure(newTree) =>
                    '{
                      $collect.map(terms => {
                        val iter = terms.iterator
                        ${ Block(makeVariables('iter), newTree).asExpr }.asInstanceOf[t]
                      }).asInstanceOf[ZIO[?, ?, t]]
                    }
                  case IR.Monad(newTree) =>
                    '{
                      $collect.flatMap(terms => {
                        val iter = terms.iterator
                        ${ Block(makeVariables('iter), newTree).asExpr }.asInstanceOf[ZIO[?, ?, t]]
                      }).asInstanceOf[ZIO[?, ?, t]]
                    }

          apply(IR.Monad(output.asTerm))
      }

  }

}
