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
    def applyFlatMap(monadExprType: ZioType)(monadExpr: Term, applyLambda: Term): Term =
      monadExprType.valueType match
        case '[t] =>
          '{
            ${ monadExpr.asExpr }.asInstanceOf[ZIO[?, ?, t]].flatMap(
              ${ applyLambda.asExprOf[t => ZIO[?, ?, ?]] }
            )
          }.asTerm

    def applyFlatMapWithBody(monadExprType: ZioType)(monadExpr: Term, valSymbol: Option[Symbol], bodyExpr: Term): Term = {
      val applyLambda =
        '{
          // make the lambda accept anything because the symbol-type computations for what `t` is are not always correct for what `t` is are not always
          // maybe something like this is needed for the flatMap case too?
          ${ makeLambda(TypeRepr.of[ZIO[?, ?, ?]])(bodyExpr, valSymbol).asExpr }.asInstanceOf[Any => ZIO[?, ?, ?]]
        }.asTerm

      applyFlatMap(monadExprType)(monadExpr, applyLambda)
    }

    def applyMap(monadExprType: ZioType)(monadExpr: Term, applyLambda: Term): Term =
      monadExprType.valueType match
        case '[t] =>
          '{
            ${ monadExpr.asExpr }.asInstanceOf[ZIO[?, ?, t]].map(
              ${ applyLambda.asExprOf[t => ?] }
            )
          }.asTerm

    def applyMapWithBody(monadExprType: ZioType)(monadExpr: Term, valSymbol: Option[Symbol], bodyTerm: Term): Term = {
      val applyLambda =
        '{
          // make the lambda accept anything because the symbol-type computations for what `t` is are not always correct for what `t` is are not always
          // maybe something like this is needed for the flatMap case too?
          ${ makeLambda(TypeRepr.of[Any])(bodyTerm, valSymbol).asExpr }.asInstanceOf[Any => ?]
        }.asTerm

      applyMap(monadExprType)(monadExpr, applyLambda)
    }

    def applyCatchSome(tryTermType: ZioType, resultType: ZioType)(tryTerm: Term, functionBlock: Term): Term =
      (tryTermType.asTypeTuple, resultType.asTypeTuple) match
        case (('[rr], '[er], '[ar]), ('[r], '[e], '[b])) =>
          '{
            ${ tryTerm.asExpr }.asInstanceOf[ZIO[rr, er, ar]]
              .catchSome { ${ functionBlock.asExpr }.asInstanceOf[PartialFunction[er, ZIO[r, e, b]]] }
          }.asTerm
        case ((_, _, _), (_, _, _)) =>
          report.errorAndAbort("Invalid match case, this shuold not be possible")

    def applyEnsuring(monadTermType: ZioType)(monadTerm: Term, finallyTerm: Term): Term =
      monadTermType.asTypeTuple match
        case ('[r], '[e], '[a]) =>
          // when generalizing to non-zio check there result-type and change ZIO[?, ?, ?] representation to the appropriate one for the given type
          '{ ${ monadTerm.asExpr }.asInstanceOf[ZIO[r, e, a]].ensuring(ZioUtil.wrapWithThrowable(${ finallyTerm.asExprOf[ZIO[?, ?, ?]] }).orDie).asInstanceOf[ZIO[r, e, a]] }.asTerm

    def applyFlatten(resultType: ZioType)(block: Term): Term =
      // when generalizing to non-zio check there result-type and change ZIO[?, ?, ?] representation to the appropriate one for the given type
      '{ ZIO.succeed(${ block.asExprOf[ZIO[?, ?, ?]] }).flatten }.asTerm

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
