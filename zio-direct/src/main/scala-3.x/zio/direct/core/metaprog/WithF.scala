package zio.direct.core.metaprog

import scala.quoted._
import zio.direct._

trait WithF {

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  class DirectMonad[F[_, _, _]: Type](
      val Success: Expr[MonadSuccess[F]],
      val Failure: Expr[MonadFallible[F]],
      val Sequence: Expr[MonadSequence[F]],
      val SequencePar: Expr[MonadSequenceParallel[F]]
  ) { self =>
    object Value {
      def succeed(term: Term) =
        term.tpe.asType match
          case '[t] =>
            '{ ${ self.Success }.unit[t](${ term.asExprOf[t] }) }

      def True =
        import quotes.reflect._
        succeed(Expr(true).asTerm)
      def False =
        import quotes.reflect._
        succeed(Expr(false).asTerm)
    }
  }

  object DirectMonad {
    // def of[F[_, _, _]: Type] = {
    def of = {
      val monadSuccess: Expr[MonadSuccess[zio.ZIO]] = '{ zio.direct.zioMonadSuccess }
      val monadFailure: Expr[MonadFallible[zio.ZIO]] = '{ zio.direct.zioMonadFallible }
      val monadSequence: Expr[MonadSequence[zio.ZIO]] = '{ zio.direct.zioMonadSequence }
      val monadSequencePar: Expr[MonadSequenceParallel[zio.ZIO]] = '{ zio.direct.zioMonadSequenceParallel }
      DirectMonad[ZIO](monadSuccess, monadFailure, monadSequence, monadSequencePar)
    }
  }

}
