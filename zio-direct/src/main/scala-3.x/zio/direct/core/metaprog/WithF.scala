package zio.direct.core.metaprog

import scala.quoted._
import zio.direct._
import zio.direct.Dsl.DirectMonadInput

trait WithF {

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
    def of[F[_, _, _]: Type, S: Type, W: Type](directMonadInput: DirectMonadInput[F, S, W]) = {
      val monadSuccess: Expr[MonadSuccess[F]] = directMonadInput.success
      val monadFailure: Option[Expr[MonadFallible[F]]] =
        Expr.summon[MonadFallible[F]]

      val monadSequence: Expr[MonadSequence[F]] = directMonadInput.sequence
      val monadSequencePar: Expr[MonadSequenceParallel[F]] = directMonadInput.sequencePar
      val monadState: Option[Expr[MonadState[F, S]]] =
        if (!(TypeRepr.of[S] =:= TypeRepr.of[Nothing]))
          Some(
            // TODO Better error
            '{ ${ directMonadInput.state }.get }
          )
        else
          None

      val monadLog: Option[Expr[MonadLog[F, W]]] =
        if (!(TypeRepr.of[W] =:= TypeRepr.of[Nothing]))
          Some(
            // TODO Better error
            '{ ${ directMonadInput.log }.get }
          )
        else
          None

      DirectMonad[F, S, W](monadSuccess, monadFailure, monadSequence, monadSequencePar, monadState, monadLog)
    }
  }

}
