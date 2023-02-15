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
      isFalliable: Boolean,
      isStateful: Boolean,
      isLogging: Boolean
  )

  private def constBoolean[T: Type]: Option[Boolean] =
    val flagType = TypeRepr.of[T]
    if (flagType =:= TypeRepr.of[true]) Some(true)
    else if (flagType =:= TypeRepr.of[false]) Some(false)
    else None

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
          constBoolean[flag] match
            case Some(value) => value
            case None        => report.errorAndAbort(s"The type IsFallible needs to be specified on the Monad-Model: ${monadModelType.show}.")

    val isStateful =
      Type.of[MM] match
        case '[MonadModel { type IsStateful = flag }] =>
          constBoolean[flag] match
            case Some(value) => value
            case None        => report.errorAndAbort(s"The type IsStateful needs to be specified on the Monad-Model: ${monadModelType.show}.")

    val isLogging =
      Type.of[MM] match
        case '[MonadModel { type IsLogging = flag }] =>
          constBoolean[flag] match
            case Some(value) => value
            case None        => report.errorAndAbort(s"The type IsLogging needs to be specified on the Monad-Model: ${monadModelType.show}.")

    MonadModelData(monadModelVariancesList, monadModelLettersList, isFallible, isStateful, isLogging)
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
          Some(directMonadInput.fallible.asExprOf[MonadFallible[F]])
        else
          None

      val monadSequence: Expr[MonadSequence[F]] = directMonadInput.sequence
      val monadSequencePar: Expr[MonadSequenceParallel[F]] = directMonadInput.sequencePar
      val monadState: Option[Expr[MonadState[F, S]]] =
        if (monadModelData.isStateful)
          Some(directMonadInput.state.asExprOf[MonadState[F, S]])
        else
          None

      val monadLog: Option[Expr[MonadLog[F, W]]] =
        if (monadModelData.isStateful)
          Some(directMonadInput.log.asExprOf[MonadLog[F, W]])
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
