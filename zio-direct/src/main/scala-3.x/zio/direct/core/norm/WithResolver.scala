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
    def applyFlatMap(monadExpr: Term, applyLambda: Term) = {
      val flatMapSig = Select.unique(monadExpr, "flatMap")
      // val firstParam = flatMapMethod.symbol.paramSymss(0)(0)
      // val firstParamType = TypeTree.ref(firstParam).tpe
      val flatMapMethod =
        TypeApply(
          // still need to do .asInstanceOf[ZIO[?, ?, t]] otherwise various tests will fail because .asExprOf is used everywhere
          flatMapSig,
          List(CommonTypes.inf, CommonTypes.inf, CommonTypes.inf)
          // List(Inferred(TypeRepr.of[AnyKind]), Inferred(TypeRepr.of[AnyKind]), Inferred(TypeRepr.of[AnyKind]))
          // List(TypeTree.ref(Wildcard().symbol), TypeTree.ref(Wildcard().symbol), TypeTree.ref(Wildcard().symbol))
        )

      val flatMapMethodApply = flatMapMethod.etaExpand(Symbol.spliceOwner)
      val valDef =
        flatMapMethodApply match
          case Lambda(List(valDef), term) => valDef
          case _                          => report.errorAndAbort("Eta-expanded flatMap is not of correct form")

      val firstParamType = valDef.tpt.tpe
      Apply(
        Apply(
          flatMapMethod,
          List({
            // whatever the type of the flatMap is supposed to be cast it to that
            firstParamType.asType match
              case '[t] =>
                '{ ${ applyLambda.asExpr }.asInstanceOf[t] }.asTerm
          })
        ),
        List(Expr.summon[zio.Trace].get.asTerm)
      )
    }

    def applyFlatMapWithBody(monadExpr: Term, valSymbol: Option[Symbol], bodyExpr: Term) = {
      val applyLambda =
        '{
          // make the lambda accept anything because the symbol-type computations for what `t` is are not always correct for what `t` is are not always
          // maybe something like this is needed for the flatMap case too?
          ${ makeLambda(TypeRepr.of[ZIO[?, ?, ?]])(bodyExpr, valSymbol).asExpr }.asInstanceOf[Any => ZIO[?, ?, ?]]
        }.asTerm

      applyFlatMap(monadExpr, applyLambda)
    }

    def applyMap(monadExpr: Term, applyLambda: Term) = {
      val out =
        Apply(
          Apply(
            TypeApply(
              // still need to do .asInstanceOf[ZIO[?, ?, t]] otherwise various tests will fail because .asExprOf is used everywhere
              Select.unique(monadExpr, "map"),
              List(CommonTypes.inf)
            ),
            List(applyLambda)
          ),
          List(Expr.summon[zio.Trace].get.asTerm)
        )
      println(s"---------------- HERE: ${monadExpr.show} ------------------")
      out
    }

    def applyMapWithBody(monadExpr: Term, valSymbol: Option[Symbol], bodyTerm: Term) = {
      val applyLambda =
        '{
          // make the lambda accept anything because the symbol-type computations for what `t` is are not always correct for what `t` is are not always
          // maybe something like this is needed for the flatMap case too?
          ${ makeLambda(TypeRepr.of[Any])(bodyTerm, valSymbol).asExpr }.asInstanceOf[Any => ?]
        }.asTerm

      applyMap(monadExpr, applyLambda)
    }

    def applyCatchSome(monadExpr: Term, partialFunctionLambda: Term) = {
      // val anyToNothing = TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])
      // val catchSomeMethod =
      //   TypeApply(
      //     Select.unique(monadExpr, "catchSome"),
      //     List(Inferred(anyToNothing), Inferred(anyToNothing), Inferred(anyToNothing))
      //   )

      // val catchSomeMethodApply = catchSomeMethod.etaExpand(Symbol.spliceOwner)
      // val valDef =
      //   catchSomeMethodApply match
      //     case Lambda(List(valDef), term) => valDef
      //     case _                          => report.errorAndAbort("Eta-expanded catchSome is not of correct form")

      // val firstParamType = valDef.tpt.tpe
      // println(s"===== First ValDef Param type: ${valDef.tpt.tpe.show}")
      // Apply(
      //   Apply(
      //     catchSomeMethod,
      //     List({
      //       // whatever the type of the flatMap is supposed to be cast it to that
      //       firstParamType.asType match
      //         case '[t] =>
      //       '{ ${ partialFunctionLambda.asExpr }.asInstanceOf[PFT] }.asTerm
      //     })
      //   ),
      //   List('{ zio.CanFail }.asTerm, Expr.summon[zio.Trace].get.asTerm)
      // )
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
