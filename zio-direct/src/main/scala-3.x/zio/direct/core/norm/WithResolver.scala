package zio.direct.core.norm

import scala.quoted._
import zio.direct.core.metaprog.WithIR
import zio.direct.core.metaprog.WithF
import zio.direct.core.metaprog.Embedder._
import zio.direct.core.metaprog.WithPrintIR
import zio.Chunk
import zio.direct.core.util.Format
import zio.direct.core.util.WithInterpolator
import zio.direct.core.metaprog.Instructions
import zio.direct.core.metaprog.Collect
import zio.direct.core.metaprog.WithZioType
import zio.direct.core.util.ZioUtil
import zio.direct.core.util.Unsupported
import org.scalafmt.util.LogLevel.info
import zio.direct.core.metaprog.Collect.Sequence
import zio.direct.core.metaprog.Collect.Parallel
import zio.direct.MonadSuccess
import zio.direct.MonadFallible
import zio.direct.MonadSequence
import zio.direct.core.util.F3Util
import zio.direct.MonadSequenceParallel
import zio.direct.core.metaprog.TypeUnion
import zio.NonEmptyChunk

trait WithResolver {
  self: WithF with WithIR with WithZioType =>

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  case class ParallelBlockExtract(monadExpr: ZioValue, monadSymbol: Symbol, tpe: TypeRepr)

  private object CommonTypes {
    val anyToNothing = TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])
    val inf = Inferred(anyToNothing)
  }

  // Right now typing this at WithReconstructTree but really will need to get it from input signatures
  class Resolver[F[_, _, _]: Type](zpe: ZioType, directMonad: DirectMonad[F]) {
    private def notPossible() =
      report.errorAndAbort("Invalid match case, this shuold not be possible")

    def applyFlatMap(monad: ZioValue, applyLambda: ZioValue): ZioValue =
      val MonadSuccess = directMonad.Success
      (monad.zpe.asTypeTuple, zpe.valueType) match
        case (('[r], '[e], '[a]), '[b]) =>
          '{
            $MonadSuccess.flatMap[r, e, a, b](${ monad.term.asExprOf[F[r, e, a]] })( // .asInstanceOf[F[r, e, a]]
              ${ applyLambda.term.asExpr }.asInstanceOf[a => F[r, e, b]])
          }.toZioValue(zpe)

    def applyFlatMapWithBody(monad: ZioValue, valSymbol: Option[Symbol], body: ZioValue): ZioValue = {
      val applyLambdaTerm = {
        body.zpe.asTypeTuple match
          case ('[r], '[e], '[a]) =>
            makeLambda(TypeRepr.of[F[r, e, a]])(body.term, valSymbol)
      }
      applyFlatMap(monad, ZioValue(applyLambdaTerm, body.zpe))
    }

    def applyMap(monad: ZioValue, applyLambdaTerm: Term): ZioValue =
      val MonadSuccess = directMonad.Success
      (monad.zpe.asTypeTuple, zpe.asTypeTuple) match
        case (('[r], '[e], '[a]), ('[or], '[oe], '[b])) =>
          val out = '{
            $MonadSuccess.map[r, e, a, b](${ monad.term.asExprOf[F[r, e, a]] })( // .asInstanceOf[F[r, e, a]]
              ${ applyLambdaTerm.asExpr }.asInstanceOf[a => b])
          }
          out.toZioValue(zpe)
        case _ =>
          notPossible()

    def applyMapWithBody(monad: ZioValue, valSymbol: Option[Symbol], bodyTerm: Term): ZioValue = {
      val applyLambdaTerm = makeLambda(TypeRepr.of[Any])(bodyTerm, valSymbol)
      applyMap(monad, applyLambdaTerm)
    }

    def applyCatchSome(tryClause: ZioValue, body: ZioValue): ZioValue = {
      val MonadFailure = directMonad.Failure
      (zpe.asTypeTuple) match
        case ('[or], '[oe], '[oa]) =>
          '{
            $MonadFailure.catchSome[or, oe, oa](${ tryClause.term.asExprOf[F[or, oe, oa]] })( // .asInstanceOf[F[or, oe, oa]]
              ${ body.term.asExpr }.asInstanceOf[PartialFunction[oe, F[or, oe, oa]]])
          }.toZioValue(zpe)
        case _ =>
          notPossible()
    }

    def applyEnsuring(monad: ZioValue, finalizer: ZioValue): ZioValue =
      val MonadFailure = directMonad.Failure
      monad.zpe.asTypeTuple match
        case ('[r], '[e], '[a]) =>
          // when generalizing to non-zio check there result-type and change ZIO[?, ?, ?] representation to the appropriate one for the given type
          '{
            $MonadFailure.ensuring(${ monad.term.asExprOf[F[r, e, a]] })(
              // TODO make a better check here, manually check if it's a subtype of throwable and cast it, otherwise make the macro fail
              $MonadFailure.orDie(
                F3Util.wrapWithThrowable[F, r, Nothing, Any](${ finalizer.term.asExprOf[F[r, Nothing, Any]] })($MonadFailure)
              )
            ).asInstanceOf[F[r, e, a]]
          }.toZioValue(zpe)

    def applyForeach(monadExpr: ZioValue, elementSymbol: Symbol, bodyMonad: ZioValue)(implicit collectStrategy: Collect) =
      val elementType = elementSymbol.termRef.widenTermRefByName.asType
      val MonadSuccess = directMonad.Success
      (zpe.asTypeTuple, elementType, bodyMonad.zpe.asTypeTuple) match
        case (('[or], '[oe], '[oa]), '[e], ('[br], '[be], '[b])) =>
          collectStrategy match
            case Sequence =>
              val MonadSequence = directMonad.Sequence
              '{
                $MonadSuccess.map[or, oe, Iterable[b], Unit](
                  $MonadSuccess.flatMap[or, oe, Iterable[e], Iterable[b]](${ monadExpr.expr }.asInstanceOf[F[or, oe, Iterable[e]]])((list: Iterable[e]) =>
                    $MonadSequence.foreach(list.asInstanceOf[Iterable[e]])(
                      ${ makeLambda(TypeRepr.of[F[or, oe, b]])(bodyMonad.term, Some(elementSymbol)).asExpr }.asInstanceOf[e => F[or, oe, b]]
                      // ${ replaceSymbolInBodyMaybe(using macroQuotes)(bodyMonad.term.changeOwner(('v).asTerm.symbol))(Some(elementSymbol), ('v).asTerm).asExprOf[ZIO[?, ?, ?]] }
                    )
                  )
                )(_ => ())
              }.toZioValue(zpe)
            case Parallel =>
              val MonadSequencePar = directMonad.SequencePar
              '{
                $MonadSuccess.map[or, oe, Iterable[b], Unit](
                  $MonadSuccess.flatMap[or, oe, Iterable[e], Iterable[b]](${ monadExpr.expr }.asInstanceOf[F[or, oe, Iterable[e]]])((list: Iterable[e]) =>
                    $MonadSequencePar.foreachPar(list.asInstanceOf[Iterable[e]])(
                      ${ makeLambda(TypeRepr.of[F[or, oe, b]])(bodyMonad.term, Some(elementSymbol)).asExpr }.asInstanceOf[e => F[or, oe, b]]
                      // ${ replaceSymbolInBodyMaybe(using macroQuotes)(bodyMonad.term.changeOwner(('v).asTerm.symbol))(Some(elementSymbol), ('v).asTerm).asExprOf[ZIO[?, ?, ?]] }
                    )
                  )
                )(_ => ())
              }.toZioValue(zpe)
        case _ =>
          notPossible()

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
      val MonadSuccess = directMonad.Success
      block.zpe.asTypeTuple match
        case ('[r], '[e], '[a]) =>
          '{
            $MonadSuccess.flatten(
              $MonadSuccess.unit(${ block.term.asExprOf[F[r, e, a]] })
            )
          }.toZioValue(zpe)

    def applyExtractedUnlifts(aliasedTree: IRT.Leaf, unlifts: List[ParallelBlockExtract], collectStrategy: Collect)(implicit tu: TypeUnion) = {
      val unliftTriples = unlifts.map(Tuple.fromProductTyped(_))
      val (terms, names, types) = unliftTriples.unzip3
      val termsTotalType =
        // If the list of unlifts is non-empty then compose it to get the type, otherwise use the output-type of the full expression
        NonEmptyChunk.fromIterableOption(terms.map(_.zpe)) match
          case Some(v) => ZioType.composeN(v)
          case None    => zpe

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
                  val MonadSequence = directMonad.Sequence
                  // Since the things inside the `run` calls on a single line could be totally unrelated e.g:
                  // ((foo: F[Foo]).run, (bar: F[Bar].run)) it make no sense to try to find a type that represents
                  // what would be the type of ZIO.collect(foo, bar) so we just use wilcards and explicitly re-type
                  // these things later.
                  '{ $MonadSequence.collectAll(Chunk.from($termsExpr)) }
                case Collect.Parallel =>
                  val MonadSequencePar = directMonad.SequencePar
                  '{ $MonadSequencePar.collectAllPar(Chunk.from($termsExpr)) }

            def makeVariables(iterator: Expr[Iterator[?]]) =
              unliftTriples.map((monad, symbol, tpe) =>
                tpe.asType match {
                  case '[t] =>
                    ValDef(symbol, Some('{ $iterator.next().asInstanceOf[t] }.asTerm))
                }
              )

            val MonadSuccess = directMonad.Success
            aliasedTree.zpe.transformA(_.widen).asTypeTuple match
              case ('[r], '[e], '[t]) =>
                aliasedTree match
                  case IRT.Pure(code) =>
                    '{
                      $MonadSuccess.map($collect)(terms => {
                        val iter = terms.iterator
                        ${ Block(makeVariables('iter), code).asExpr }.asInstanceOf[t]
                      }).asInstanceOf[F[r, e, t]]
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
