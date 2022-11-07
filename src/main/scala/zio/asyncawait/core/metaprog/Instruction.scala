package zio.asyncawait.core.metaprog

import scala.quoted._

case class Instructions(info: InfoBehavior)

sealed trait InfoBehavior
object InfoBehavior {
  case object Silent extends InfoBehavior
  case object Info extends InfoBehavior
  case object Verbose extends InfoBehavior
}

object MakeInstructions {
  def fromExpr(expr: Expr[Instructions])(using Quotes) =
    import quotes.reflect._
    val infoBehaviorExpr =
      expr match {
        case '{ Instructions($info) } => info
        case _ => report.throwError(s"Illegal instructions expression: ${expr}")
      }
    val infoBehavior =
      infoBehaviorExpr match {
        case '{ InfoBehavior.Silent } => InfoBehavior.Silent
        case '{ InfoBehavior.Info } => InfoBehavior.Info
        case '{ InfoBehavior.Verbose } => InfoBehavior.Verbose
      }
    Instructions(infoBehavior)
}
