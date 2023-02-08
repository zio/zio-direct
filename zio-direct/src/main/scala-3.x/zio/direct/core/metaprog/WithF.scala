package zio.direct.core.metaprog

import scala.quoted._
import zio.direct._

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
    def of[F[_, _, _]: Type, S: Type, W: Type] = {
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
      val monadState: Option[Expr[MonadState[F, S]]] =
        if (!(TypeRepr.of[S] =:= TypeRepr.of[Nothing]))
          Some(Expr.summon[MonadState[F, S]].getOrElse {
            report.errorAndAbort(
              s"""|Expected an implicit MonadState[${TypeRepr.of[F].show}, ${TypeRepr.of[S].show}] to exist on the context but it was not found.
                  |For zio-direct-streams: `import zio.direct.stream._`
                  |For zio-direct-pure:    `import zio.direct.pure._`
                  |""".stripMargin
            )
          })
        else
          None

      val monadLog: Option[Expr[MonadLog[F, W]]] =
        if (!(TypeRepr.of[W] =:= TypeRepr.of[Nothing]))
          Some(Expr.summon[MonadLog[F, W]].getOrElse {
            report.errorAndAbort(
              s"""|Expected an implicit MonadLog[${TypeRepr.of[F].show}, ${TypeRepr.of[W].show}] to exist on the context but it was not found.
                  |For zio-direct-streams: `import zio.direct.stream._`
                  |For zio-direct-pure:    `import zio.direct.pure._`
                  |""".stripMargin
            )
          })
        else
          None

      DirectMonad[F, S, W](monadSuccess, monadFailure, monadSequence, monadSequencePar, monadState, monadLog)
    }
  }

}
