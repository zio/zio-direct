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
import zio.direct.MonadSuccess
import zio.direct.MonadFallible
import zio.direct.MonadSequence
import zio.direct.core.util.F3Util
import zio.direct.MonadSequenceParallel
import zio.direct.core.metaprog.TypeUnion

trait WithResolver {
  self: WithIR with WithZioType =>

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  case class ParallelBlockExtract(monadExpr: ZioValue, monadSymbol: Symbol, tpe: TypeRepr)
  class Capabilities[F[_, _, _]](success: Expr[MonadSuccess[F]], failure: Expr[MonadFallible[F]], sequence: Expr[MonadSequence[F]])

  // TODO Memoize by passing an instance of this
  object SummonCapability {
    def Success[F[_, _, _]: Type]: Expr[MonadSuccess[F]] =
      Expr.summon[MonadSuccess[F]].getOrElse {
        report.errorAndAbort(s"Cannot perform map/flatMap/succeed on the type: ${TypeRepr.of[F].show}. MonadSuccess typeclass was not found.")
      }
    def Failure[F[_, _, _]: Type]: Expr[MonadFallible[F]] =
      Expr.summon[MonadFallible[F]].getOrElse {
        report.errorAndAbort(s"Cannot find an implementation of type typeclass MonadFallible for: ${TypeRepr.of[F].show}")
      }
    def Sequence[F[_, _, _]: Type]: Expr[MonadSequence[F]] =
      Expr.summon[MonadSequence[F]].getOrElse {
        report.errorAndAbort(s"Cannot find an implementation of type typeclass MonadSequence for: ${TypeRepr.of[F].show}")
      }
    def SequencePar[F[_, _, _]: Type]: Expr[MonadSequenceParallel[F]] =
      Expr.summon[MonadSequenceParallel[F]].getOrElse {
        report.errorAndAbort(s"Cannot find an implementation of type typeclass MonadSequence for: ${TypeRepr.of[F].show}")
      }
  }

  private object CommonTypes {
    val anyToNothing = TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])
    val inf = Inferred(anyToNothing)
  }

  // Right now typing this at WithReconstructTree but really will need to get it from input signatures
  class Resolver[F[_, _, _]: Type](zpe: ZioType) {
    private def notPossible() =
      report.errorAndAbort("Invalid match case, this shuold not be possible")

    def applyFlatMap(monad: ZioValue, applyLambda: ZioValue): ZioValue =
      val MonadSuccess = SummonCapability.Success[F]
      (monad.zpe.asTypeTuple, zpe.valueType) match
        case (('[r], '[e], '[a]), '[b]) =>
          '{
            $MonadSuccess.flatMap[r, e, a, b](${ monad.term.asExpr }.asInstanceOf[F[r, e, a]])(
              ${ applyLambda.term.asExpr }.asInstanceOf[a => F[r, e, b]]
            )
          }.toZioValue(zpe)

    def applyFlatMapWithBody(monad: ZioValue, valSymbol: Option[Symbol], body: ZioValue): ZioValue = {
      val applyLambdaTerm = {
        body.zpe.asTypeTuple match
          case ('[r], '[e], '[a]) =>
            '{
              // make the lambda accept anything because the symbol-type computations for what `t` is are not always correct for what `t` is are not always
              // maybe something like this is needed for the flatMap case too?
              ${ makeLambda(TypeRepr.of[F[r, e, a]])(body.term, valSymbol).asExpr }.asInstanceOf[Any => F[r, e, a]]
            }.asTerm
      }
      applyFlatMap(monad, ZioValue(applyLambdaTerm, body.zpe))
    }

    def applyMap(monad: ZioValue, applyLambdaTerm: Term): ZioValue =
      val MonadSuccess = SummonCapability.Success[F]
      (monad.zpe.asTypeTuple, zpe.asTypeTuple) match
        case (('[r], '[e], '[a]), ('[or], '[oe], '[b])) =>
          // println(s"------------------ Cast To: ${TypeRepr.of[ZIO[or, oe, b]].show} ----------------")
          val out = '{
            $MonadSuccess.map[r, e, a, b](${ monad.term.asExpr }.asInstanceOf[F[r, e, a]])(
              ${ applyLambdaTerm.asExpr }.asInstanceOf[a => b]
            )
          }
          // val castedMonad = '{ ${ monad.term.asExpr }.asInstanceOf[ZIO[or, oe, oa]] }.asTerm
          // println(
          //   s"============ Monad type: ${monad.term.tpe.widen.show} =========\n" +
          //     monad.term.show + "\n" +
          //     s"============ Monad type-casted: ${castedMonad.tpe.show}} =========\n" +
          //     castedMonad.show + "\n" +
          //     s"============ Lambda type: ${applyLambdaTerm.asExprOf[t => ?].asTerm.tpe.show} =========\n" +
          //     s"============ out: ${out.asTerm.tpe.show} ============\n" +
          //     out.asTerm.show + "\n"
          // )
          out.toZioValue(zpe)
        case _ =>
          notPossible()

    def applyMapWithBody(monad: ZioValue, valSymbol: Option[Symbol], bodyTerm: Term): ZioValue = {
      val applyLambdaTerm =
        '{
          // make the lambda accept anything because the symbol-type computations for what `t` is are not always correct for what `t` is are not always
          // maybe something like this is needed for the flatMap case too?
          ${ makeLambda(TypeRepr.of[Any])(bodyTerm, valSymbol).asExpr }.asInstanceOf[Any => ?]
        }.asTerm

      applyMap(monad, applyLambdaTerm)
    }

    def applyCatchSome(tryClause: ZioValue, body: ZioValue): ZioValue = {
      val MonadFailure = SummonCapability.Failure[F]
      (tryClause.zpe.asTypeTuple, body.zpe.asTypeTuple) match
        case (('[rr], '[er], '[ar]), ('[r], '[e], '[b])) =>
          '{
            ${ tryClause.term.asExpr }.asInstanceOf[ZIO[rr, er, ar]]
              .catchSome { ${ body.term.asExpr }.asInstanceOf[PartialFunction[er, ZIO[r, e, b]]] }
          }.toZioValue(zpe)
        case _ =>
          notPossible()
    }

    def applyEnsuring(monad: ZioValue, finalizer: ZioValue): ZioValue =
      val MonadFailure = SummonCapability.Failure[F]
      monad.zpe.asTypeTuple match
        case ('[r], '[e], '[a]) =>
          // when generalizing to non-zio check there result-type and change ZIO[?, ?, ?] representation to the appropriate one for the given type
          '{
            $MonadFailure.ensuring(${ monad.term.asExpr }.asInstanceOf[F[r, e, a]])(
              // TODO make a better check here, manually check if it's a subtype of throwable and cast it, otherwise make the macro fail
              $MonadFailure.orDie(
                F3Util.wrapWithThrowable[F, r, Nothing, Any](${ finalizer.term.asExprOf[F[r, Nothing, Any]] })($MonadFailure)
              )
            ).asInstanceOf[ZIO[r, e, a]]
          }.toZioValue(zpe)

    def applyFlatten(block: ZioValue): ZioValue =
      /*
      This is for situations where you have a block that returns a ZIO value but does not necessarily have all of it's
      terms wrapped. For example:
        defer {
          val x = {
            val a = "foo"
            if (a == something()) throw new RuntimeException("Can't be that")
            ZIO.succeed(a)
          }
          x.run
        }
      In this sutation, despite the fact that the inner block returns a ZIO-value, it still needs to be wrapped into a ZIO
      since otherwise the exception would escape the ZIO effect-system.
       */
      // when generalizing to non-zio check there result-type and change ZIO[?, ?, ?] representation to the appropriate one for the given type
      val MonadSuccess = SummonCapability.Success[F]
      block.zpe.asTypeTuple match
        case ('[r], '[e], '[a]) =>
          '{
            $MonadSuccess.flatten(
              $MonadSuccess.unit(${ block.term.asExpr }.asInstanceOf[F[r, e, a]])
            )
          }.toZioValue(zpe)

    def applyExtractedUnlifts(aliasedTree: IRT.Leaf, unlifts: List[ParallelBlockExtract], collectStrategy: Collect)(implicit tu: TypeUnion) = {
      val unliftTriples = unlifts.map(Tuple.fromProductTyped(_))
      val (terms, names, types) = unliftTriples.unzip3
      val termsTotalType = ZioType.composeN(terms.map(_.zpe))

      val output =
        termsTotalType.asTypeTuple match
          case ('[tr], '[te], '[ta]) => {
            // Since the things inside the `run` calls on a single line could be totally unrelated e.g:
            // ((foo: F[Foo]).run, (bar: F[Bar].run)) we compute the total widened type of all of them, this is usually Nothing
            // and we cast each of them to that here. Then later when we deal with pulling them out of the iterator
            // we cast them back to their real types (that we keep from the `types` variable i.e. the ParallelBlockExtract.tpe vars)
            val termsExpr = Expr.ofList(terms.map(zval => '{ ${ zval.term.asExpr }.asInstanceOf[F[tr, te, ta]] }))
            val collect =
              collectStrategy match
                case Collect.Sequence =>
                  val MonadSequence = SummonCapability.Sequence[F]
                  // Since the things inside the `run` calls on a single line could be totally unrelated e.g:
                  // ((foo: F[Foo]).run, (bar: F[Bar].run)) it make no sense to try to find a type that represents
                  // what would be the type of ZIO.collect(foo, bar) so we just use wilcards and explicitly re-type
                  // these things later.
                  '{ $MonadSequence.collectAll(Chunk.from($termsExpr)) }
                case Collect.Parallel =>
                  val MonadSequencePar = SummonCapability.SequencePar[F]
                  '{ $MonadSequencePar.collectAllPar(Chunk.from($termsExpr)) }

            def makeVariables(iterator: Expr[Iterator[?]]) =
              unliftTriples.map((monad, symbol, tpe) =>
                tpe.asType match {
                  case '[t] =>
                    ValDef(symbol, Some('{ $iterator.next().asInstanceOf[t] }.asTerm))
                }
              )

            val MonadSuccess = SummonCapability.Success[F]
            aliasedTree.zpe.transformA(_.widen).asTypeTuple match
              case ('[r], '[e], '[t]) =>
                aliasedTree match
                  case IRT.Pure(code) =>
                    '{
                      $MonadSuccess.map($collect)(terms => {
                        val iter = terms.iterator
                        ${ Block(makeVariables('iter), code).asExpr }.asInstanceOf[t]
                      }).asInstanceOf[ZIO[r, e, t]]
                    }
                  case IRT.Monad(code, _) =>
                    '{
                      $MonadSuccess.flatMap($collect)(terms => {
                        val iter = terms.iterator
                        ${ Block(makeVariables('iter), code).asExpr }.asInstanceOf[F[tr, te, t]]
                      }).asInstanceOf[F[r, e, t]]
                    }
          }

      output.asTerm.toZioValue(zpe)
    }

    def makeLambda(outputType: TypeRepr)(body: Term, prevValSymbolOpt: Option[Symbol]) = {
      val prevValSymbolType =
        prevValSymbolOpt match {
          case Some(oldSymbol) => oldSymbol.termRef.widenTermRefByName
          case None            => TypeRepr.of[Any]
        }

      val mtpe = MethodType(List("sm"))(_ => List(prevValSymbolType), _ => outputType)
      println(s"lambda-type:  => ${outputType.show}") // ${inputType.show}

      Lambda(
        Symbol.spliceOwner,
        mtpe,
        {
          case (methSym, List(sm: Term)) =>
            replaceSymbolInBodyMaybe(using macroQuotes)(body)(prevValSymbolOpt, sm).changeOwner(methSym)
          case _ =>
            report.errorAndAbort("Not a possible state")
        }
      )
    }
  }
}
