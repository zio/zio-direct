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

trait WithResolver {
  self: WithIR with WithZioType with WithComputeType with WithPrintIR with WithInterpolator =>

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  private object CommonTypes {
    val anyToNothing = TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])
    val inf = Inferred(anyToNothing)
  }

  object Resolver {
    def applyFlatMap(monad: ZioValue, applyLambda: ZioValue): Term =
      monad.zpe.valueType match
        case '[t] =>
          '{
            ${ monad.term.asExpr }.asInstanceOf[ZIO[?, ?, t]].flatMap(
              ${ applyLambda.term.asExprOf[t => ZIO[?, ?, ?]] }
            )
          }.asTerm

    def applyFlatMapWithBody(monad: ZioValue, valSymbol: Option[Symbol], body: ZioValue): Term = {
      val applyLambdaTerm =
        '{
          // make the lambda accept anything because the symbol-type computations for what `t` is are not always correct for what `t` is are not always
          // maybe something like this is needed for the flatMap case too?
          ${ makeLambda(TypeRepr.of[ZIO[?, ?, ?]])(body.term, valSymbol).asExpr }.asInstanceOf[Any => ZIO[?, ?, ?]]
        }.asTerm

      applyFlatMap(monad, ZioValue(applyLambdaTerm, body.zpe))
    }

    def applyMap(monad: ZioValue, applyLambdaTerm: Term): Term =
      monad.zpe.valueType match
        case '[t] =>
          '{
            ${ monad.term.asExpr }.asInstanceOf[ZIO[?, ?, t]].map(
              ${ applyLambdaTerm.asExprOf[t => ?] }
            )
          }.asTerm

    def applyMapWithBody(monad: ZioValue, valSymbol: Option[Symbol], bodyTerm: Term): Term = {
      val applyLambdaTerm =
        '{
          // make the lambda accept anything because the symbol-type computations for what `t` is are not always correct for what `t` is are not always
          // maybe something like this is needed for the flatMap case too?
          ${ makeLambda(TypeRepr.of[Any])(bodyTerm, valSymbol).asExpr }.asInstanceOf[Any => ?]
        }.asTerm

      applyMap(monad, applyLambdaTerm)
    }

    def applyCatchSome(resultType: ZioType)(tryClause: ZioValue, body: ZioValue): Term =
      (tryClause.zpe.asTypeTuple, resultType.asTypeTuple) match
        case (('[rr], '[er], '[ar]), ('[r], '[e], '[b])) =>
          '{
            ${ tryClause.term.asExpr }.asInstanceOf[ZIO[rr, er, ar]]
              .catchSome { ${ body.term.asExpr }.asInstanceOf[PartialFunction[er, ZIO[r, e, b]]] }
          }.asTerm
        case ((_, _, _), (_, _, _)) =>
          report.errorAndAbort("Invalid match case, this shuold not be possible")

    def applyEnsuring(monad: ZioValue, finalizer: ZioValue): Term =
      monad.zpe.asTypeTuple match
        case ('[r], '[e], '[a]) =>
          // when generalizing to non-zio check there result-type and change ZIO[?, ?, ?] representation to the appropriate one for the given type
          '{ ${ monad.term.asExpr }.asInstanceOf[ZIO[r, e, a]].ensuring(ZioUtil.wrapWithThrowable(${ finalizer.term.asExprOf[ZIO[?, ?, ?]] }).orDie).asInstanceOf[ZIO[r, e, a]] }.asTerm

    def applyFlatten(block: ZioValue): Term =
      // when generalizing to non-zio check there result-type and change ZIO[?, ?, ?] representation to the appropriate one for the given type
      '{ ZIO.succeed(${ block.term.asExprOf[ZIO[?, ?, ?]] }).flatten }.asTerm

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
