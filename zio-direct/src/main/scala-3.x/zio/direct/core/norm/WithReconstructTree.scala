package zio.direct.core.norm

import zio.direct.core.metaprog.WithIR
import scala.quoted._
import zio.direct.core.metaprog.Embedder._
import zio.ZIO
import zio.direct.core.metaprog.WithPrintIR
import zio.Chunk
import zio.direct.core.util.Format
import zio.direct.core.util.WithInterpolator
import zio.Exit.Success
import zio.Exit.Failure
import zio.direct.core.metaprog.Instructions
import zio.direct.core.metaprog.Collect
import zio.direct.core.metaprog.WithZioType
import zio.direct.core.util.ZioUtil
import zio.direct.core.util.Unsupported
import org.scalafmt.util.LogLevel.info
import zio.direct.core.metaprog.Collect.Sequence
import zio.direct.core.metaprog.Collect.Parallel
import java.lang.reflect.WildcardType

trait WithReconstructTree {
  self: WithIR with WithZioType with WithComputeType with WithPrintIR with WithInterpolator with WithResolver =>

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

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
    //       oldSymbol.termRef.widenTermRefByName
    //     case None =>
    //       alternativeSource.tpe.asType match
    //         case '[ZIO[r, e, a]] => TypeRepr.of[a]

    private def compressBlock(accum: List[Statement] = List(), block: IR.Block): (List[Statement], Term) =
      block.tail match
        case nextBlock: IR.Block =>
          compressBlock(accum :+ block.head, nextBlock)
        case otherMonad =>
          (accum :+ block.head, apply(otherMonad))

    private def apply(ir: IR, isTopLevel: Boolean = false): Term = {
      ir match
        case IR.Pure(code) => ZioApply.succeed(code)

        // IR.Unsafe is a construct used to modify the internal tree via the WrapUnsafes phase.
        // As such, we don't need to anything special with it during tree-reconstruction.
        case IR.Unsafe(body) =>
          apply(body)

        // Pull out the value from IR.Pure and use it directly in the mapping
        case IR.Map(monad, valSymbol, IR.Pure(bodyTerm)) =>
          val monadExpr = apply(monad)
          Resolver.applyMapWithBody(ComputeType.fromIR(monad))(monadExpr, valSymbol, bodyTerm)

        case IR.FlatMap(monad, valSymbol, body) => {
          val monadTerm = apply(monad)
          val bodyTerm = apply(body)
          // Symbol type needs to be the same as the A-parameter of the ZIO, if not it's an error
          // should possibly introduce an asserition for that
          // Also:
          // TODO synthesize + eta-expand the lambda manually so it's ame is based on the previous symbol name
          Resolver.applyFlatMapWithBody(ComputeType.fromIR(monad))(monadTerm, valSymbol, bodyTerm)
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
          val monadExpr = apply(listIR).asExpr
          val bodyMonad = apply(body).asExpr
          val elementType = elementSymbol.termRef.widenTermRefByName.asType
          (listType.widen.asType, elementType) match
            case ('[l], '[e]) =>
              instructions.collect match
                case Sequence =>
                  '{
                    $monadExpr.asInstanceOf[ZIO[?, ?, l]].flatMap((list: l) =>
                      ZIO.foreach(list.asInstanceOf[Iterable[e]])((v: e) =>
                        ${ replaceSymbolInBodyMaybe(using macroQuotes)(bodyMonad.asTerm.changeOwner(('v).asTerm.symbol))(Some(elementSymbol), ('v).asTerm).asExprOf[ZIO[?, ?, ?]] }
                      ).map(_ => ())
                    )
                  }.asTerm
                case Parallel =>
                  '{
                    $monadExpr.asInstanceOf[ZIO[?, ?, l]].flatMap((list: l) =>
                      ZIO.foreachPar(list.asInstanceOf[Iterable[e]])((v: e) =>
                        ${ replaceSymbolInBodyMaybe(using macroQuotes)(bodyMonad.asTerm.changeOwner(('v).asTerm.symbol))(Some(elementSymbol), ('v).asTerm).asExprOf[ZIO[?, ?, ?]] }
                      ).map(_ => ())
                    )
                  }.asTerm

        case IR.Monad(code) => code

        case IR.Fail(error) =>
          error match {
            case IR.Pure(value) =>
              ComputeType.fromIR(error).a.widen.asType match
                case '[a] =>
                  '{ ZIO.fail[a](${ value.asExpr }.asInstanceOf[a]) }.asTerm
            // TODO test the case where error is constructed via an run
            case m: IR.Monadic =>
              ComputeType.fromIR(error).asTypeTuple match
                case ('[r], '[e], '[a]) =>
                  val monad = apply(m)
                  monad.tpe.asType match
                    case '[t] =>
                      Resolver.applyFlatMap(ComputeType.fromIR(m))(monad, '{ (err: Any) => ZIO.fail(err) }.asTerm)
          }

        case block: IR.Block =>
          val blockTermType = ComputeType.fromIR(block)
          val (stmts, term) = compressBlock(List(), block)

          // if we are on the top-level we are not inside of any map or flatMap
          // which means that we need to nest any possible exceptions into ZIO.succeed
          // so that they will go into the effect system instead of directly to the outside
          val blockTerm = Block(stmts, term)
          if (isTopLevel)
            val zioModule = Symbol.requiredModule("zio.ZIO")
            val zioModuleUnit = Select.unique(Ref(zioModule), "unit")
            Resolver.applyFlatten(blockTermType)(blockTerm)
          else
            blockTerm

        case value: IR.Match =>
          reconstructMatch(value)

        case value: IR.If =>
          reconstructIf(value)

        case expr @ IR.And(left, right) =>
          val leftType = ComputeType.fromIR(left)
          (left, right) match {
            case (a: IR.Monadic, b: IR.Monadic) =>
              Resolver.applyFlatMap(leftType)(apply(a), '{ (r: Any) => r match { case true => ${ apply(b).asExpr }; case false => ${ ZioApply.False.asExpr } } }.asTerm)
            case (a: IR.Monadic, IR.Pure(b)) =>
              Resolver.applyMap(leftType)(apply(a), '{ (r: Any) => r match { case true => ${ b.asExpr }; case false => ${ ZioApply.False.asExpr } } }.asTerm)
            case (IR.Pure(a), b: IR.Monadic) =>
              '{
                if (${ a.asExprOf[Boolean] }) ${ apply(b).asExpr }
                else ${ ZioApply.False.asExpr }
              }.asTerm
            // case Pure/Pure is taken care by in the transformer on a higher-level via the PureTree case. Still, handle them just in case
            case (IR.Pure(a), IR.Pure(b)) =>
              ZioApply.succeed('{ ${ a.asExprOf[Boolean] } && ${ b.asExprOf[Boolean] } }.asTerm)
            case _ => report.errorAndAbort(s"Invalid boolean variable combination:\n${PrintIR(expr)}")
          }

        case expr @ IR.Or(left, right) =>
          val leftType = ComputeType.fromIR(left)
          (left, right) match {
            case (a: IR.Monadic, b: IR.Monadic) =>
              Resolver.applyFlatMap(leftType)(apply(a), '{ (r: Any) => r match { case true => ${ ZioApply.True.asExpr }; case false => ${ apply(b).asExpr } } }.asTerm)
            case (a: IR.Monadic, IR.Pure(b)) =>
              Resolver.applyMap(leftType)(apply(a), '{ (r: Any) => r match { case true => ${ ZioApply.True.asExpr }; case false => ${ b.asExpr } } }.asTerm)
            case (IR.Pure(a), b: IR.Monadic) =>
              '{
                if (${ a.asExprOf[Boolean] }) ${ ZioApply.True.asExpr }
                else ${ apply(b).asExpr }
              }.asTerm
            // case Pure/Pure is taken care by in the transformer on a higher-level via the PureTree case. Still, handle them just in case
            case (IR.Pure(a), IR.Pure(b)) =>
              '{ ZIO.succeed(${ a.asExprOf[Boolean] } || ${ b.asExprOf[Boolean] }) }.asTerm
            case _ => report.errorAndAbort(s"Invalid boolean variable combination:\n${PrintIR(expr)}")
          }

        // TODO test with dependencyes and various errors types in both condition and body
        case irWhile @ IR.While(whileCond, whileBody) =>
          // Since the function needs to know the type for the definition (especially because it's recursive!) need to find that out here
          val methOutputComputed = ComputeType.fromIR(irWhile)
          val methOutputTpe = methOutputComputed.toZioType

          val methodType = MethodType(Nil)(_ => Nil, _ => methOutputTpe)
          // println(s"========== Output Method Type: ${methOutputTpe.show} =====")

          val methSym = Symbol.newMethod(Symbol.spliceOwner, "whileFunc", methodType)

          val newMethodBody =
            IR.If(
              whileCond,
              IR.FlatMap(
                IR.Monad(apply(whileBody)), // apply().asTerm.changeOwner(methSym),
                None,
                IR.Monad(Apply(Ref(methSym), Nil))
              ),
              IR.Pure('{ () }.asTerm)
            )
          // val newMethodBodyExpr =
          //   methOutputComputed.asTypeTuple match
          //     case ('[r], '[e], '[a]) =>
          //       apply(newMethodBody)

          val newMethodBodyExpr =
            methOutputTpe.asType match
              case '[t] =>
                val newMethodBodyTerm = apply(newMethodBody)
                '{ ${ newMethodBodyTerm.asExpr }.asInstanceOf[t] }.asTerm

          val newMethod = DefDef(methSym, sm => Some(newMethodBodyExpr))
          // Block(List(newMethod), Apply(Ref(methSym), Nil)).asExprOf[ZIO[?, ?, ?]]
          apply(IR.Block(newMethod, IR.Monad(Apply(Ref(methSym), Nil))))

        case tryIR @ IR.Try(tryBlock, cases, _, finallyBlock) =>
          val tryBlockType = ComputeType.fromIR(tryBlock)
          val newCaseDefs = reconstructCaseDefs(cases)
          val resultType = ComputeType.fromIR(tryIR)
          val tryTerm = apply(tryBlock)

          (tryBlockType.toZioType.asType, tryBlockType.e.asType, resultType.toZioType.asType) match
            case ('[zioTry], '[zioTry_E], '[zioOut]) =>
              println(s"========== IR.Try try block type: ${tryBlockType.toZioType.show} =====")

              println(s"========== IR.Try result block type: ${resultType.toZioType.show} =====")

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
              val methodType = MethodType(List("tryLamParam"))(_ => List(TypeRepr.of[zioTry_E]), _ => TypeRepr.of[zioOut])

              println(s"           Method input=>output: ${methodType.show} =====")

              val methSym = Symbol.newMethod(Symbol.spliceOwner, "tryLam", methodType)

              // Now we actually make the method with the body:
              //   def tryLamParam(tryLam: e) = { case ...exception => ... }
              val method = DefDef(methSym, sm => Some(Match(sm(0)(0).asInstanceOf[Term], newCaseDefs.map(_.changeOwner(methSym)))))
              // NOTE: Be sure that error here is the same one as used to define tryLamParam. Otherwise, AbstractMethodError errors
              // saying that .isDefinedAt is abstract will happen.
              val pfTree = TypeRepr.of[PartialFunction[zioTry_E, zioOut]]

              // Assemble the peices together into a closure
              val closure = Closure(Ref(methSym), Some(pfTree))
              val functionBlock = '{ ${ Block(List(method), closure).asExpr }.asInstanceOf[PartialFunction[zioTry_E, zioOut]] }
              val tryExpr = '{ ${ tryTerm.asExpr }.asInstanceOf[zioTry] }
              // val monadExpr = '{ ${ tryTerm.asExpr }.asInstanceOf[zioRET].catchSome { ${ functionBlock } } }
              val monadTerm = Resolver.applyCatchSome(tryBlockType, resultType)(tryExpr.asTerm, functionBlock.asTerm)

              finallyBlock match {
                case Some(ir) =>
                  val finallyTerm = apply(ir)
                  Resolver.applyEnsuring(resultType)(monadTerm, finallyTerm)

                case None =>
                  monadTerm
              }

        case value: IR.Parallel =>
          reconstructParallel(value)
    }

    def reconstructMatch(value: IR.Match): Term =
      val IR.Match(scrutinee, caseDefs) = value
      scrutinee match
        case value: IR.Monadic =>
          val monadExpr = apply(value)
          // use the symbol instead of the monad as the scrutinee because the monad needs to be flatmapped first
          // (Note don't think you need to change ownership of caseDef.rhs to the new symbol but possible.)
          val matchSymbol = Symbol.newVal(Symbol.spliceOwner, "matchVar", monadExpr.tpe, Flags.EmptyFlags, Symbol.noSymbol)
          val newCaseDefs = reconstructCaseDefs(caseDefs)
          // Possible exploration: if the content of the match is pure we lifted it into a monad. If we want to optimize we
          // change the IR.CaseDef.rhs to be IR.Pure as well as IR.Monadic and handle both cases
          val newMatch = Match(Ref(matchSymbol), newCaseDefs)
          // We can synthesize the monadExpr.flatMap call from the monad at this point but I would rather pass it to the FlatMap case to take care of
          apply(IR.FlatMap(IR.Monad(monadExpr), matchSymbol, IR.Monad(newMatch)))
        case IR.Pure(termValue) =>
          val newCaseDefs = reconstructCaseDefs(caseDefs)
          val newMatch = Match(termValue, newCaseDefs)
          // recall that the expressions in the case defs need all be ZIO instances (i.e. monadic) we we can
          // treat the whole thing as a ZIO (i.e. monadic) expression
          newMatch
    end reconstructMatch

    private def reconstructCaseDefs(caseDefs: List[IR.Match.CaseDef]) =
      caseDefs.map { caseDef =>
        val bodyExpr = apply(caseDef.rhs)
        CaseDef(caseDef.pattern, caseDef.guard, bodyExpr)
      }

    def reconstructIf(value: IR.If): Term =
      val IR.If(cond, ifTrue, ifFalse) = value
      enum ConditionState:
        case BothPure(ifTrue: Term, ifFalse: Term)
        case BothMonadic(ifTrue: IR.Monadic, ifFalse: IR.Monadic)

      val conditionState =
        (ifTrue, ifFalse) match {
          case (IR.Pure(a), IR.Pure(b))       => ConditionState.BothPure(a, b)
          case (a: IR.Pure, b: IR.Monadic)    => ConditionState.BothMonadic(IR.Monad(apply(a)), b)
          case (a: IR.Monadic, b: IR.Pure)    => ConditionState.BothMonadic(a, IR.Monad(apply(b)))
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
              val ifTrueTerm = apply(ifTrue)
              val ifFalseTerm = apply(ifFalse)
              apply(IR.FlatMap(m, sym, IR.Monad(If(Ref(sym), ifTrueTerm, ifFalseTerm))))
            // For example: if(run(something)) "foo" else "bar"
            case ConditionState.BothPure(ifTrue, ifFalse) =>
              apply(IR.Map(m, sym, IR.Pure(If(Ref(sym), ifTrue, ifFalse))))
          }
        }
        case IR.Pure(value) => {
          conditionState match {
            case ConditionState.BothMonadic(ifTrue, ifFalse) =>
              val ifTrueTerm = apply(ifTrue)
              val ifFalseTerm = apply(ifFalse)
              If(value, ifTrueTerm, ifFalseTerm)
            case ConditionState.BothPure(ifTrue, ifFalse) =>
              val ifStatement = If(value, ifTrue, ifFalse)
              apply(IR.Pure(ifStatement))
          }
        }
      }
    end reconstructIf

    def reconstructParallel(value: IR.Parallel): Term =
      val IR.Parallel(_, unlifts, newTree) = value
      unlifts.toList match {
        case List() =>
          newTree match
            case IR.Pure(newTree)  => ZioApply.succeed(newTree)
            case IR.Monad(newTree) => newTree

        /*
        For a expression (in a single block-line) that has one run in the middle of things e.g.
        { run(foo) + bar }
        Needs to turn into something like:
        { run(foo).map(fooVal => fooVal + bar) }
        When thinking about types, it looks something like:
        { (run(foo:Task[t]):t + bar):r }
        { run(foo:t):Task[t].map[r](fooVal:t => (fooVal + bar):r) }
         */
        case List((monad, oldSymbol)) => {
          // wrap a body of code that pointed to some identifier whose symbol is prevValSymbol
          def wrapWithLambda(inputType: TypeRepr, outputType: TypeRepr)(body: Term, prevValSymbol: Symbol) = {
            // Can use the type of the previous value symbol because as the input to the lambda that is what it was prior to being spliced
            // For example, this:
            //   (foo:Foo, ZIO.succeed(bar:Bar).run)
            // would become:
            //   ZIO.succeed(bar:Bar).run.map(v:VVV => (foo: Foo, v:VVV)) so the type of VVV is Bar
            val mtpe = MethodType(List("sm"))(_ => List(inputType), _ => outputType)
            println(s"lambda-type: ${mtpe.show}")
            Lambda(
              Symbol.spliceOwner,
              mtpe,
              {
                case (methSym, List(sm: Term)) =>
                  replaceSymbolIn(body)(prevValSymbol, sm).changeOwner(methSym)
                case _ =>
                  report.errorAndAbort("Not a possible state")
              }
            )
          }

          println(s"Inside IR (map): ${PrintIR(monad)}")

          val monadExpr = apply(monad)
          val monadType = ComputeType.fromIR(monad)

          (monadExpr.tpe.asType) match {
            case '[ZIO[x, y, t]] =>
              println(s"Monad Expr Type: ${monadExpr.tpe.show} output type: ${TypeRepr.of[ZIO[x, y, t]].show}")

              newTree match {
                case IR.Pure(newTree) =>
                  newTree.tpe.widenTermRefByName.asType match {
                    case '[r] =>
                      // println(s"expected lambda (map): ${TypeRepr.of[t].show} => ${TypeRepr.of[r].show}")
                      val lamRaw = wrapWithLambda(monadType.a, TypeRepr.of[r])(newTree, oldSymbol)
                      // Often the Scala compiler doesn't know what the the real output type of the castMonadExpr which is the input type of `lam`
                      // so we cast it to an existential type here
                      val lam = '{ ${ lamRaw.asExpr }.asInstanceOf[Any => r] }
                      // println(s"------------ wrapped lambda (map): ${lam.asTerm.tpe.show}")
                      // val mappedMonadExpr = monadExpr.asExprOf[ZIO[?, ?, t]]
                      val castMonadExpr = '{ ${ monadExpr.asExpr }.asInstanceOf[ZIO[?, ?, Any]] }
                      // println(s"=========== monad expr type (map): ${castMonadExpr.asTerm.tpe.show}")

                      val output = apply(IR.Monad(Resolver.applyMap(monadType)(monadExpr, lam.asTerm)))
                      // println(s"output type (map): ${output.tpe.show}")
                      println(s"========== output ZIO Type (map): ${output.tpe.show}")
                      output
                  }
                case IR.Monad(newTree) =>
                  newTree.tpe.widenTermRefByName.asType match {
                    case '[zr] =>
                      // newTree.tpe.asType match {
                      // case '[ZIO[x1, y1, r]] =>

                      val lamRaw = wrapWithLambda(monadType.a, newTree.tpe)(newTree, oldSymbol)
                      val lam = '{ ${ lamRaw.asExpr }.asInstanceOf[Any => zr] }

                      apply(IR.Monad(Resolver.applyFlatMap(monadType)(monadExpr, lam.asTerm)))
                    // could also work like this?
                    // apply(IR.Monad('{ ${monadExpr.asExprOf[ZIO[Any, Nothing, t]]}.flatMap[Any, Nothing, r](${lam.asExprOf[t => ZIO[Any, Nothing, r]]}) }.asTerm))
                    // apply(IR.Monad('{ ${ monadExpr.asExprOf[ZIO[?, ?, t]] }.flatMap(${ lam.asExprOf[t => zr] }) }.asTerm))
                  }
              }
          }
        }
        case unlifts =>
          val unliftTriples =
            unlifts.map((monad, monadSymbol) => {
              val monadExpr = apply(monad)
              val tpe = monadSymbol.termRef.widenTermRefByName
              (monadExpr, monadSymbol, tpe)
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
