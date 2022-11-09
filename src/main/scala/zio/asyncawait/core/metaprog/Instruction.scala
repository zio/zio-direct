package zio.asyncawait.core.metaprog

import scala.quoted._
import zio.asyncawait.core.util.Format

case class Instructions(info: InfoBehavior, collect: Collect)

sealed trait InfoBehavior {
  def showComputedType: Boolean
  def showDeconstructed: Boolean
  def showReconstructed: Boolean
  def showReconstructedTree: Boolean
}
object InfoBehavior {
  case object Silent extends InfoBehavior {
    val showComputedType = false
    val showDeconstructed = false
    val showReconstructed = false
    val showReconstructedTree = false
  }
  case object Info extends InfoBehavior {
    val showComputedType = true
    val showDeconstructed = true
    val showReconstructed = false
    val showReconstructedTree = false
  }
  case object Verbose extends InfoBehavior {
    val showComputedType = true
    val showDeconstructed = true
    val showReconstructed = true
    val showReconstructedTree = false
  }
  case object VerboseTree extends InfoBehavior {
    val showComputedType = true
    val showDeconstructed = true
    val showReconstructed = true
    val showReconstructedTree = true
  }
}

sealed trait Collect
object Collect {
  case object Sequence extends Collect
  case object Parallel extends Collect
}

object Unliftables {
  def unliftCollect(collect: Expr[Collect])(using Quotes) =
    Implicits.unliftCollect.unliftOrfail(collect)

  def unliftInfoBehavior(info: Expr[InfoBehavior])(using Quotes) =
    Implicits.unliftInfoBehavior.unliftOrfail(info)

  private object Implicits {
    extension [T](expr: Expr[T])(using unlifter: Unlifter[T], q: Quotes)
      def fromExpr = unlifter.unliftOrfail(expr)

    trait Unlifter[T] extends FromExpr[T]:
      def tpe: Quotes ?=> Type[T]
      def unlift: Quotes ?=> PartialFunction[Expr[T], T]
      def unapply(v: Expr[T])(using Quotes): Option[T] =
        unlift.lift(v)
      def unliftOrfail(v: Expr[T])(using Quotes) =
        import quotes.reflect._
        unlift.lift(v).getOrElse {
          report.errorAndAbort(s"Could not unlift the expression ${Format.Expr(v)} into the type: ${Format.Type(tpe)}")
        }

    given unliftCollect: Unlifter[Collect] with {
      def tpe = Type.of[Collect]
      def unlift =
        case '{ Collect.Sequence } => Collect.Sequence
        case '{ Collect.Parallel } => Collect.Parallel
    }

    given unliftInfoBehavior: Unlifter[InfoBehavior] with {
      def tpe = Type.of[InfoBehavior]
      def unlift =
        case '{ InfoBehavior.Info } => InfoBehavior.Info
        case '{ InfoBehavior.Silent } => InfoBehavior.Silent
        case '{ InfoBehavior.Verbose } => InfoBehavior.Verbose
        case '{ InfoBehavior.VerboseTree } => InfoBehavior.VerboseTree
    }
  }
}
