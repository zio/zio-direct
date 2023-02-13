package zio.direct.core.metaprog

import scala.quoted._
import zio.direct._
import zio.direct.Dsl.DirectMonadInput

trait MacroBase {
  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  case class MonadModelData(
      variancesListType: TypeRepr,
      lettersListType: TypeRepr,
      isFalliable: Boolean
  )

  def computeMonadModelData[MM <: MonadModel: Type]: MonadModelData = {
    // dealias the type e.g. ZioMonadType to get the Variance/Letters variables underneath
    val monadModelType = TypeRepr.of[MM].dealias

    val monadModelVariancesList =
      Type.of[MM] match
        case '[MonadModel { type Variances = list }] => TypeRepr.of[list]

    val monadModelLettersList =
      Type.of[MM] match
        case '[MonadModel { type Letters = list }] => TypeRepr.of[list]

    val isFallible =
      Type.of[MM] match
        case '[MonadModel { type IsFallible = flag }] =>
          val flagType = TypeRepr.of[flag]
          if (flagType =:= TypeRepr.of[true]) true
          else if (flagType =:= TypeRepr.of[false]) false
          else report.errorAndAbort(s"The type IsFallible needs to be specified on the Monad-Model: ${monadModelType.show}. Currently: ${flagType.show}")

    MonadModelData(monadModelVariancesList, monadModelLettersList, isFallible)
  }
}

trait WithF extends MacroBase {
  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  class DirectMonad[F[_, _, _]: Type, S: Type, W: Type](
      val Success: Expr[MonadSuccess[F]],
      val Failure: Option[Expr[MonadFallible[F]]],
      val Sequence: Expr[MonadSequence[F]],
      val SequencePar: Expr[MonadSequenceParallel[F]],
      val MonadState: Option[Expr[MonadState[F, S]]],
      val MonadLog: Option[Expr[MonadLog[F, W]]]
  ) { self =>
    object Value {
      def succeed(term: Term) =
        term.tpe.asType match
          case '[t] =>
            val typedTerm = term.asExprOf[t]
            '{ ${ self.Success }.unit[t]($typedTerm) }

      def True =
        import quotes.reflect._
        succeed(Expr(true).asTerm)
      def False =
        import quotes.reflect._
        succeed(Expr(false).asTerm)
    }
  }

  object DirectMonad {
    def of[F[_, _, _]: Type, S: Type, W: Type](directMonadInput: DirectMonadInput[F, S, W], monadModelData: MonadModelData) = {
      lazy val printEffect = Expr(TypeRepr.of[F].show)
      lazy val printState = Expr(TypeRepr.of[S].show)
      lazy val printLog = Expr(TypeRepr.of[W].show)

      val monadSuccess: Expr[MonadSuccess[F]] = directMonadInput.success
      val monadFailure: Option[Expr[MonadFallible[F]]] =
        if (monadModelData.isFalliable)
          directMonadInput.fallible match {
            case '{ Some($value) }   => Some(value.asExprOf[MonadFallible[F]])
            case '{ Option($value) } => Some(value.asExprOf[MonadFallible[F]])
            case _ =>
              Some('{
                ${ directMonadInput.fallible }.getOrElse {
                  throw new IllegalArgumentException(ErrorForWithF.make($printEffect, "", "MonadFallible"))
                }
              })
          }
        else
          None

      val monadSequence: Expr[MonadSequence[F]] = directMonadInput.sequence
      val monadSequencePar: Expr[MonadSequenceParallel[F]] = directMonadInput.sequencePar
      val monadState: Option[Expr[MonadState[F, S]]] =
        if (!(TypeRepr.of[S] =:= TypeRepr.of[Nothing]))
          directMonadInput.state match {
            case '{ Some($value) }   => Some(value.asExprOf[MonadState[F, S]])
            case '{ Option($value) } => Some(value.asExprOf[MonadState[F, S]])
            case _ =>
              Some(
                '{
                  ${ directMonadInput.state }.getOrElse {
                    throw new IllegalArgumentException(ErrorForWithF.make($printEffect, $printState, "MonadState"))
                  }
                }
              )
          }
        else
          None

      val monadLog: Option[Expr[MonadLog[F, W]]] =
        if (!(TypeRepr.of[W] =:= TypeRepr.of[Nothing]))
          directMonadInput.log match
            case '{ Some($value) }   => Some(value.asExprOf[MonadLog[F, W]])
            case '{ Option($value) } => Some(value.asExprOf[MonadLog[F, W]])
            case _ =>
              Some(
                '{
                  ${ directMonadInput.log }.getOrElse {
                    throw new IllegalArgumentException(ErrorForWithF.make($printEffect, $printLog, "MonadLog"))
                  }
                }
              )
        else
          None

      DirectMonad[F, S, W](monadSuccess, monadFailure, monadSequence, monadSequencePar, monadState, monadLog)
    }
  }

}

object ErrorForWithF {
  def make(effectType: String, otherType: String, monadStateOrLogName: String) =
    val addition = if (otherType == "") "" else s", $otherType"
    s"""Expected an input $monadStateOrLogName[$effectType$addition] to exist on the context but it was not found.""".stripMargin
}
