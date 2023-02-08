package zio.direct.core.metaprog

import scala.quoted._
import zio.direct._

trait WithF {

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  class DirectMonad[F[_, _, _]: Type](
      val Success: Expr[MonadSuccess[F]],
      val Failure: Option[Expr[MonadFallible[F]]],
      val Sequence: Expr[MonadSequence[F]],
      val SequencePar: Expr[MonadSequenceParallel[F]],
      val MonadState: Option[Expr[MonadState[F]]],
      val MonadLog: Option[Expr[MonadLog[F]]]
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
    def of[F[_, _, _]: Type] = {
      val monadSuccess: Expr[MonadSuccess[F]] =
        Expr.summon[MonadSuccess[F]].getOrElse {
          report.errorAndAbort(s"Cannot perform map/flatMap/succeed on the type: ${TypeRepr.of[F].show}. A MonadSuccess typeclass was not found for it.")
        }
      val monadFailure: Option[Expr[MonadFallible[F]]] =
        Expr.summon[MonadFallible[F]]
      val monadSequence: Expr[MonadSequence[F]] =
        Expr.summon[MonadSequence[F]].getOrElse {
          report.errorAndAbort(s"Cannot perform collect/foreach on the type: ${TypeRepr.of[F].show}. A SequencePar typeclass was not found for it.")
        }
      val monadSequencePar: Expr[MonadSequenceParallel[F]] =
        Expr.summon[MonadSequenceParallel[F]].getOrElse {
          report.errorAndAbort(s"Cannot perform collectPar/foreachPar on the type: ${TypeRepr.of[F].show}. A SequencePar typeclass was not found for it.")
        }
      val monadState: Option[Expr[MonadState[F]]] =
        Expr.summon[MonadState[F]]
      val monadLog: Option[Expr[MonadLog[F]]] =
        Expr.summon[MonadLog[F]]

      DirectMonad[F](monadSuccess, monadFailure, monadSequence, monadSequencePar, monadState, monadLog)
    }
  }

}
