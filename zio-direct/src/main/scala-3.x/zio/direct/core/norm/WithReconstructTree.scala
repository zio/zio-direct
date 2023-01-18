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
import zio.direct.core.metaprog.TypeUnion

trait WithReconstructTree {
  self: WithIR with WithZioType with WithComputeType with WithPrintIR with WithInterpolator with WithResolver =>

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  protected object ReconstructTree {
    def apply(instructions: Instructions) =
      new ReconstructTree(instructions)
  }

  implicit class TermOps(term: Term) {
    def toZioValue(zpe: ZioType) = ZioValue(term, zpe)
  }

  protected class ReconstructTree private (instructions: Instructions) {
    implicit val instructionsInst: Instructions = instructions
    // so that we can do IRT.Compute
    implicit val typeUnion: TypeUnion = instructions.typeUnion

    def fromIR(irt: IRT) = apply(irt, true).term

    // private def computeSymbolType(valSymbol: Option[Symbol], alternativeSource: Term) =
    //   valSymbol match
    //     // TODO Add a verification here that the alternativeSource [a] symbol has has the same type as valSymbol?
    //     case Some(oldSymbol) =>
    //       oldSymbol.termRef.widenTermRefByName
    //     case None =>
    //       alternativeSource.tpe.asType match
    //         case '[ZIO[r, e, a]] => TypeRepr.of[a]

    private def compressBlock(accum: List[Statement] = List(), block: IRT.Block): (List[Statement], Term) =
      block.tail match
        case nextBlock: IRT.Block =>
          compressBlock(accum :+ block.head, nextBlock)
        case otherMonad =>
          (accum :+ block.head, apply(otherMonad).term)

    private def apply(ir: IRT, isTopLevel: Boolean = false): ZioValue = {
      ir match
        case irt: IRT.Pure =>
          ZioValue(irt.zpe).succeed(irt.code)

        // IRT.Unsafe is a construct used to modify the internal tree via the WrapUnsafes phase.
        // As such, we don't need to anything special with it during tree-reconstruction.
        case irt: IRT.Unsafe =>
          apply(irt.body)

        // Pull out the value from IRT.Pure and use it directly in the mapping
        case irt @ IRT.Map(monad, valSymbol, body) =>
          Resolver.applyMapWithBody(apply(monad), valSymbol, body.code).toZioValue(irt.zpe)

        case irt @ IRT.FlatMap(monad, valSymbol, body) => {
          // Symbol type needs to be the same as the A-parameter of the ZIO, if not it's an error
          // should possibly introduce an asserition for that
          // Also:
          // TODO synthesize + eta-expand the lambda manually so it's ame is based on the previous symbol name
          Resolver.applyFlatMapWithBody(apply(monad), valSymbol, apply(body)).toZioValue(irt.zpe)
        }

        case irt @ IRT.ValDef(originalStmt, symbol, assignment, bodyUsingVal) =>
          (assignment, bodyUsingVal) match {
            // E.g: { val x = 123; somethingPure } - If it is a totally pure value, just return the original statement
            // case (_: IRT.Pure, _: IRT.Pure) => Some(origStmt)
            case (_: IRT.Pure, _: IRT.Pure) =>
              ZioValue(irt.zpe).succeed(irt.originalStmt)
            // The following cases are possible:
            // 0. Pure/Pure     - { val x = 123; somethingPure }
            // 1. Pure/Impure   - { val x = 123; somethingMonadic }
            // 2. Impure/Pure   - { val x = succeed(123).run; somethingPure }
            // 3. Impure/Impure - { val x = succeed(123).run; somethingMonadic }
            case (_, pureBody: IRT.Pure) =>
              // remove the 1st case by making the assingment monadic (wrap it if needed)
              Resolver.applyMapWithBody(apply(assignment), Some(symbol), pureBody.code).toZioValue(irt.zpe)
            // apply(IRT.Map(IRT.Monad(apply(assignment)), irt.symbol, pureBody).typed)

            case (_, monadicBody: IRT.Monadic) =>
              Resolver.applyFlatMapWithBody(apply(assignment), Some(symbol), apply(monadicBody)).toZioValue(irt.zpe)
          }

        case irt @ IRT.Foreach(listIR, listType, elementSymbol, body) =>
          // For something like
          //   (list:Iterable[E]).foreach(e => body)
          // the `sym` is of type E because it is `e`
          // We need to transform it into something like:
          //   ZIO.succeed(list).map { (l:Iterable[E] => ZIO.foreach(l)(body) }
          val monadExpr = apply(listIR)
          val bodyMonad = apply(body)
          val elementType = elementSymbol.termRef.widenTermRefByName.asType
          (listType.widen.asType, elementType) match
            case ('[l], '[e]) =>
              instructions.collect match
                case Sequence =>
                  '{
                    ${ monadExpr.expr }.asInstanceOf[ZIO[?, ?, l]].flatMap((list: l) =>
                      ZIO.foreach(list.asInstanceOf[Iterable[e]])((v: e) =>
                        ${ replaceSymbolInBodyMaybe(using macroQuotes)(bodyMonad.term.changeOwner(('v).asTerm.symbol))(Some(elementSymbol), ('v).asTerm).asExprOf[ZIO[?, ?, ?]] }
                      ).map(_ => ())
                    )
                  }.asTerm.toZioValue(irt.zpe)
                case Parallel =>
                  '{
                    ${ monadExpr.expr }.asInstanceOf[ZIO[?, ?, l]].flatMap((list: l) =>
                      ZIO.foreachPar(list.asInstanceOf[Iterable[e]])((v: e) =>
                        ${ replaceSymbolInBodyMaybe(using macroQuotes)(bodyMonad.term.changeOwner(('v).asTerm.symbol))(Some(elementSymbol), ('v).asTerm).asExprOf[ZIO[?, ?, ?]] }
                      ).map(_ => ())
                    )
                  }.asTerm.toZioValue(irt.zpe)

        case irt @ IRT.Monad(code, _) => code.toZioValue(irt.zpe)

        case irt @ IRT.Fail(error) =>
          error match {
            case IRT.Pure(value) =>
              error.zpe.a.widen.asType match
                case '[a] =>
                  '{ ZIO.fail[a](${ value.asExpr }.asInstanceOf[a]) }.asTerm.toZioValue(irt.zpe)

            // TODO test the case where error is constructed via an run
            case m: IRT.Monadic =>
              error.zpe.asTypeTuple match
                case ('[r], '[e], '[a]) =>
                  val monad = apply(m)
                  // TODO use monad.zpe instead since that is the actual correct type
                  monad.term.tpe.asType match
                    case '[t] =>
                      // normally irt.tpe is the value of the total expression as opposed to the head/body operations
                      // (e.g. in a flatMap(head, body)) but in this case it is the same for both
                      Resolver.applyFlatMap(monad, '{ (err: Any) => ZIO.fail(err) }.asTerm.toZioValue(irt.zpe)).toZioValue(irt.zpe)
          }

        case block: IRT.Block =>
          val zpe = block.zpe
          val (stmts, term) = compressBlock(List(), block)

          // if we are on the top-level we are not inside of any map or flatMap
          // which means that we need to nest any possible exceptions into ZIO.succeed
          // so that they will go into the effect system instead of directly to the outside
          val blockTerm = Block(stmts, term).toZioValue(zpe)
          if (isTopLevel)
            Resolver.applyFlatten(blockTerm).toZioValue(zpe)
          else
            blockTerm

        case value: IRT.Match =>
          reconstructMatch(value)

        case value: IRT.If =>
          reconstructIf(value)

        case irt @ IRT.And(left, right) =>
          val leftType = left.zpe
          (left, right) match {
            case (a: IRT.Monadic, b: IRT.Monadic) =>
              val rightExpr = '{ (r: Any) => r match { case true => ${ apply(b).expr }; case false => ${ ZioValue(left.zpe).False.expr } } }
              Resolver.applyFlatMap(apply(a), rightExpr.asTerm.toZioValue(b.zpe)).toZioValue(irt.zpe)
            case (a: IRT.Monadic, IRT.Pure(b)) =>
              val rightExpr = '{ (r: Any) => r match { case true => ${ b.asExpr }; case false => ${ ZioValue(left.zpe).False.expr } } }
              Resolver.applyMap(apply(a), rightExpr.asTerm).toZioValue(irt.zpe)
            case (IRT.Pure(a), b: IRT.Monadic) =>
              '{
                if (${ a.asExprOf[Boolean] }) ${ apply(b).expr }
                else ${ ZioValue(b.zpe).False.expr }
              }.asTerm.toZioValue(irt.zpe)
            // case Pure/Pure is taken care by in the transformer on a higher-level via the PureTree case. Still, handle them just in case
            case (IRT.Pure(a), IRT.Pure(b)) =>
              ZioValue(irt.zpe).succeed('{ ${ a.asExprOf[Boolean] } && ${ b.asExprOf[Boolean] } }.asTerm)
            case _ =>
              report.errorAndAbort(s"Invalid boolean variable combination:\n${PrintIR(irt)}")
          }

        case irt @ IRT.Or(left, right) =>
          val leftType = left.zpe
          (left, right) match {
            case (a: IRT.Monadic, b: IRT.Monadic) =>
              Resolver.applyFlatMap(apply(a), '{ (r: Any) => r match { case true => ${ ZioValue(right.zpe).True.expr }; case false => ${ apply(b).expr } } }.asTerm.toZioValue(right.zpe)).toZioValue(irt.zpe)
            case (a: IRT.Monadic, IRT.Pure(b)) =>
              Resolver.applyMap(apply(a), '{ (r: Any) => r match { case true => ${ ZioValue(right.zpe).True.expr }; case false => ${ b.asExpr } } }.asTerm).toZioValue(irt.zpe)
            case (IRT.Pure(a), b: IRT.Monadic) =>
              '{
                if (${ a.asExprOf[Boolean] }) ${ ZioValue(b.zpe).True.expr }
                else ${ apply(b).expr }
              }.asTerm.toZioValue(irt.zpe)
            // case Pure/Pure is taken care by in the transformer on a higher-level via the PureTree case. Still, handle them just in case
            case (IRT.Pure(a), IRT.Pure(b)) =>
              '{ ZIO.succeed(${ a.asExprOf[Boolean] } || ${ b.asExprOf[Boolean] }) }.asTerm.toZioValue(irt.zpe)
            case _ =>
              report.errorAndAbort(s"Invalid boolean variable combination:\n${PrintIR(irt)}")
          }

        // TODO test with dependencyes and various errors types in both condition and body
        case irWhile @ IRT.While(whileCond, whileBody) =>
          // Since the function needs to know the type for the definition (especially because it's recursive!) need to find that out here
          val methOutputComputed = whileBody.zpe // originally was irWhile.zpe, actually I think it should be whileBody
          val methOutputTpe = methOutputComputed.toZioType

          val methodType = MethodType(Nil)(_ => Nil, _ => methOutputTpe)
          // println(s"========== Output Method Type: ${methOutputTpe.show} =====")

          val methSym = Symbol.newMethod(Symbol.spliceOwner, "whileFunc", methodType)

          // if-statement properly knows what to do if the condition is pure/monadic so use that
          val newMethodBody =
            IRT.If(
              whileCond,
              IRT.FlatMap(
                IRT.Monad.fromZioValue(apply(whileBody)), // apply().asTerm.changeOwner(methSym),
                None,
                IRT.Monad(Apply(Ref(methSym), Nil), IR.Monad.Source.Pipeline)(methOutputComputed)
              )(methOutputComputed),
              IRT.Pure.fromTerm('{ () }.asTerm)
            )(irWhile.zpe)

          // val newMethodBodyExpr =
          //   methOutputComputed.asTypeTuple match
          //     case ('[r], '[e], '[a]) =>
          //       apply(newMethodBody)

          val newMethodBodyExpr =
            methOutputTpe.asType match
              case '[t] =>
                val newMethodBodyTerm = apply(newMethodBody)
                '{ ${ newMethodBodyTerm.expr }.asInstanceOf[t] }.asTerm

          val newMethod = DefDef(methSym, sm => Some(newMethodBodyExpr))
          // Block(List(newMethod), Apply(Ref(methSym), Nil)).asExprOf[ZIO[?, ?, ?]]
          // apply(IRT.Block(newMethod, IRT.Monad(Apply(Ref(methSym), Nil))))
          Block(List(newMethod), Apply(Ref(methSym), Nil)).toZioValue(irWhile.zpe)

        case irt @ IRT.Try(tryBlock, cases, _, finallyBlock) =>
          val tryBlockType = tryBlock.zpe
          val newCaseDefs = reconstructCaseDefs(cases)
          val caseDefsType = ComputeIRT.applyCaseDefs(cases)
          val tryTerm = apply(tryBlock)

          (tryBlockType.toZioType.asType, tryBlockType.e.asType, irt.zpe.toZioType.asType) match
            case ('[zioTry], '[zioTry_E], '[zioOut]) =>
              println(s"========== IRT.Try try block type: ${tryBlockType.toZioType.show} =====")

              println(s"========== IRT.Try result block type: ${irt.zpe.toZioType.show} =====")

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
              val tryExpr = '{ ${ tryTerm.expr }.asInstanceOf[zioTry] }
              // val monadExpr = '{ ${ tryTerm.asExpr }.asInstanceOf[zioRET].catchSome { ${ functionBlock } } }
              val monadTerm = Resolver.applyCatchSome(tryExpr.asTerm.toZioValue(tryBlock.zpe), functionBlock.asTerm.toZioValue(caseDefsType))
              val monadZioValue = monadTerm.toZioValue(tryBlock.zpe.flatMappedWith(caseDefsType))

              finallyBlock match {
                case Some(ir) =>
                  val finallyTerm = apply(ir)
                  Resolver.applyEnsuring(monadZioValue, finallyTerm).toZioValue(irt.zpe)

                case None =>
                  monadZioValue
              }

        case value: IRT.Parallel =>
          reconstructParallel(value)
    }

    def reconstructMatch(irt: IRT.Match): ZioValue =
      val IRT.Match(scrutinee, caseDefs) = irt
      scrutinee match
        case value: IRT.Monadic =>
          val monad = apply(value)
          val caseDefTerms = reconstructCaseDefs(caseDefs)
          // use the symbol instead of the monad as the scrutinee because the monad needs to be flatmapped first
          // (Note don't think you need to change ownership of caseDef.rhs to the new symbol but possible.)
          val matchSymbol = Symbol.newVal(Symbol.spliceOwner, "matchVar", monad.term.tpe, Flags.EmptyFlags, Symbol.noSymbol)

          // TODO should introduce IR/IRT.CaseDefs as a separate module that can hold a type
          //      so that this doesn't need to be recomputed
          val caseDefType = ComputeIRT.applyCaseDefs(caseDefs)

          // Possible exploration: if the content of the match is pure we lifted it into a monad. If we want to optimize we
          // change the IRT.CaseDef.rhs to be IRT.Pure as well as IRT.Monadic and handle both cases
          val newMatch = Match(Ref(matchSymbol), caseDefTerms).toZioValue(caseDefType)

          // We can synthesize the monadExpr.flatMap call from the monad at this point but I would rather pass it to the FlatMap case to take care of
          Resolver.applyFlatMapWithBody(monad, Some(matchSymbol), newMatch).toZioValue(irt.zpe)

        case IRT.Pure(termValue) =>
          val newCaseDefs = reconstructCaseDefs(caseDefs)
          val newMatch = Match(termValue, newCaseDefs)
          // recall that the expressions in the case defs need all be ZIO instances (i.e. monadic) we we can
          // treat the whole thing as a ZIO (i.e. monadic) expression
          newMatch.toZioValue(irt.zpe)
    end reconstructMatch

    private def reconstructCaseDefs(caseDefs: List[IRT.Match.CaseDef]) =
      caseDefs.map { caseDef =>
        val rhs = apply(caseDef.rhs)
        CaseDef(caseDef.pattern, caseDef.guard, rhs.term)
      }

    def reconstructIf(irt: IRT.If): ZioValue =
      val IRT.If(cond, ifTrue, ifFalse) = irt
      enum ConditionState:
        case BothPure(ifTrue: Term, ifFalse: Term)
        case BothMonadic(ifTrue: IRT.Monadic, ifFalse: IRT.Monadic)

      val conditionState =
        (ifTrue, ifFalse) match {
          case (IRT.Pure(a), IRT.Pure(b))       => ConditionState.BothPure(a, b)
          case (a: IRT.Pure, b: IRT.Monadic)    => ConditionState.BothMonadic(IRT.Monad.fromZioValue(apply(a)), b)
          case (a: IRT.Monadic, b: IRT.Pure)    => ConditionState.BothMonadic(a, IRT.Monad.fromZioValue(apply(b)))
          case (a: IRT.Monadic, b: IRT.Monadic) => ConditionState.BothMonadic(a, b)
        }

      cond match {
        case m: IRT.Monadic => {
          val sym = Symbol.newVal(Symbol.spliceOwner, "ifVar", TypeRepr.of[Boolean], Flags.EmptyFlags, Symbol.noSymbol)
          conditionState match {
            // For example: if(run(something)) run(foo) else run(bar)
            // => something.map(ifVar => (foo, bar) /*replace-to-ifVar*/)
            // Note that in this case we embed foo, bar into the if-statement. They are ZIO-values which is why we need a flatMap
            case ConditionState.BothMonadic(ifTrueIRT, ifFalseIRT) =>
              val condVal = apply(m)
              val ifTrueVal = apply(ifTrue)
              val ifFalseVal = apply(ifFalse)
              val body =
                If(Ref(sym), ifTrueVal.term, ifFalseVal.term).toZioValue(
                  ZioType.compose(ifTrueVal.zpe, ifFalseVal.zpe)
                )
              Resolver.applyFlatMapWithBody(condVal, Some(sym), body).toZioValue(irt.zpe)

            // For example: if(run(something)) "foo" else "bar"
            case ConditionState.BothPure(ifTrue, ifFalse) =>
              val condVal = apply(m)
              val body = If(Ref(sym), ifTrue, ifFalse)
              Resolver.applyMapWithBody(condVal, Some(sym), body).toZioValue(irt.zpe)
          }
        }
        case IRT.Pure(value) => {
          conditionState match {
            case ConditionState.BothMonadic(ifTrue, ifFalse) =>
              val ifTrueTerm = apply(ifTrue)
              val ifFalseTerm = apply(ifFalse)
              If(value, ifTrueTerm.term, ifFalseTerm.term).toZioValue(irt.zpe)
            case ConditionState.BothPure(ifTrue, ifFalse) =>
              val ifStatement = If(value, ifTrue, ifFalse)
              ZioValue(irt.zpe).succeed(ifStatement)
          }
        }
      }
    end reconstructIf

    def reconstructParallel(irt: IRT.Parallel): ZioValue =
      val IRT.Parallel(_, unlifts, newTree) = irt
      unlifts.toList match {
        case List() =>
          newTree match
            case IRT.Pure(newTree)     => ZioValue(irt.zpe).succeed(newTree)
            case IRT.Monad(newTree, _) => newTree.toZioValue(irt.zpe)

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

          println(s"Inside IRT (map): ${PrintIR(monad)}")

          val monadVal = apply(monad)
          val monadType = monadVal.zpe

          // TODO Use monadVal.zpe instead to check what the t ype is
          (monadVal.term.tpe.asType) match {
            case '[ZIO[x, y, t]] =>
              println(s"Monad Expr Type: ${monadVal.term.tpe.show} output type: ${TypeRepr.of[ZIO[x, y, t]].show}")

              newTree match {
                case IRT.Pure(newTree) =>
                  newTree.tpe.widenTermRefByName.asType match {
                    case '[r] =>
                      // println(s"expected lambda (map): ${TypeRepr.of[t].show} => ${TypeRepr.of[r].show}")
                      val lamRaw = wrapWithLambda(monadType.a, TypeRepr.of[r])(newTree, oldSymbol)
                      // Often the Scala compiler doesn't know what the the real output type of the castMonadExpr which is the input type of `lam`
                      // so we cast it to an existential type here
                      val lam = '{ ${ lamRaw.asExpr }.asInstanceOf[Any => r] }.asTerm
                      val castMonadExpr = '{ ${ monadVal.expr }.asInstanceOf[ZIO[?, ?, Any]] }

                      val output = Resolver.applyMap(monadVal, lam)
                      // println(s"========== output ZIO Type (map): ${output.tpe.show}")
                      output.toZioValue(irt.zpe)
                  }
                case mon @ IRT.Monad(newTree, _) =>
                  newTree.tpe.widenTermRefByName.asType match {
                    case '[zr] =>
                      // newTree.tpe.asType match {
                      // case '[ZIO[x1, y1, r]] =>

                      val lamRaw = wrapWithLambda(monadType.a, newTree.tpe)(newTree, oldSymbol)
                      val lam = '{ ${ lamRaw.asExpr }.asInstanceOf[Any => zr] }.asTerm
                      val output = Resolver.applyFlatMap(monadVal, lam.toZioValue(mon.zpe))
                      output.toZioValue(irt.zpe)
                    // could also work like this?
                    // apply(IRT.Monad('{ ${monadExpr.asExprOf[ZIO[Any, Nothing, t]]}.flatMap[Any, Nothing, r](${lam.asExprOf[t => ZIO[Any, Nothing, r]]}) }.asTerm))
                    // apply(IRT.Monad('{ ${ monadExpr.asExprOf[ZIO[?, ?, t]] }.flatMap(${ lam.asExprOf[t => zr] }) }.asTerm))
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
          val termsExpr = Expr.ofList(terms.map(_.term.asExprOf[ZIO[?, ?, ?]]))
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
            newTree.zpe.transformA(_.widen).asTypeTuple match
              case ('[r], '[e], '[t]) =>
                newTree match
                  case IRT.Pure(code) =>
                    '{
                      $collect.map(terms => {
                        val iter = terms.iterator
                        ${ Block(makeVariables('iter), code).asExpr }.asInstanceOf[t]
                      }).asInstanceOf[ZIO[?, ?, t]]
                    }
                  case IRT.Monad(code, _) =>
                    '{
                      $collect.flatMap(terms => {
                        val iter = terms.iterator
                        ${ Block(makeVariables('iter), code).asExpr }.asInstanceOf[ZIO[?, ?, t]]
                      }).asInstanceOf[ZIO[?, ?, t]]
                    }

          output.asTerm.toZioValue(irt.zpe)
      }

  }

}
