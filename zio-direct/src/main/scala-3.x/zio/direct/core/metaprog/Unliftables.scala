package zio.direct.core.metaprog

import scala.quoted._
import zio.direct.core.util.Format
import zio.direct.Dsl.Params
import zio.direct.core.util.TraceType

object Unliftables {
  def unliftCollect(collect: Expr[Collect])(using Quotes) =
    Implicits.unliftCollect.unliftOrfail(collect)

  def unliftVerify(verify: Expr[Verify])(using Quotes) =
    Implicits.unliftVerify.unliftOrfail(verify)

  def unliftInfoBehavior(info: Expr[InfoBehavior])(using Quotes) =
    Implicits.unliftInfoBehavior.unliftOrfail(info)

  def unliftParams(info: Expr[Params])(using Quotes) =
    Implicits.unliftParams.unliftOrfail(info)

  def unliftTraceTypes(traceTypes: Expr[List[TraceType]])(using Quotes) =
    import Implicits.{given, _}
    traceTypes.fromExpr

  private object Implicits {
    extension [T](expr: Expr[T])(using unlifter: FromExpr[T])
      def fromExpr(using q: Quotes) = unlifter.unliftOrfail(expr)

    extension [T](unlifter: Unlifter[T])
      def unliftOrfail(v: Expr[T])(using Quotes) =
        import quotes.reflect._
        unlifter.unapply(v).getOrElse {
          report.errorAndAbort(s"Could not unlift the expression ${Format.Expr(v)} into the type: ${Format.Type(unlifter.tpe)}")
        }

    // Define an unliftOrFail for general FromExpr instances. The only difference is that we do not
    // automatically know the type of the thing we are unlifting. This makes issues a bit harder
    // to diagnost. That is why we prefer the one with Unlifter instead.
    extension [T](unlifter: FromExpr[T])
      def unliftOrfail(v: Expr[T])(using Quotes) =
        import quotes.reflect._
        unlifter.unapply(v).getOrElse {
          report.errorAndAbort(s"Could not unlift the expression ${Format.Expr(v)}.")
        }

    trait Unlifter[T] extends FromExpr[T]:
      def tpe: Quotes ?=> Type[T]
      def unlift: Quotes ?=> PartialFunction[Expr[T], T]
      def unapply(v: Expr[T])(using Quotes): Option[T] =
        unlift.lift(v)

    given unliftCollect: Unlifter[Collect] with {
      def tpe = Type.of[Collect]
      def unlift =
        case '{ Collect.Sequence } => Collect.Sequence
        case '{ Collect.Parallel } => Collect.Parallel
        case '{ Collect.default }  => Collect.default
    }

    given unliftVerify: Unlifter[Verify] with {
      def tpe = Type.of[Verify]
      def unlift =
        case '{ Verify.Strict }  => Verify.Strict
        case '{ Verify.Lenient } => Verify.Lenient
        case '{ Verify.None }    => Verify.None
        case '{ Verify.default } => Verify.default
    }

    given unliftInfoBehavior: Unlifter[InfoBehavior] with {
      def tpe = Type.of[InfoBehavior]
      def unlift =
        case '{ InfoBehavior.Info }        => InfoBehavior.Info
        case '{ InfoBehavior.Silent }      => InfoBehavior.Silent
        case '{ InfoBehavior.Verbose }     => InfoBehavior.Verbose
        case '{ InfoBehavior.VerboseTree } => InfoBehavior.VerboseTree
        case '{ InfoBehavior.default }     => InfoBehavior.default
    }

    given unliftTypeUnion: Unlifter[TypeUnion] with {
      def tpe = Type.of[TypeUnion]
      def unlift =
        case '{ TypeUnion.OrType }     => TypeUnion.OrType
        case '{ TypeUnion.LeastUpper } => TypeUnion.LeastUpper
        case '{ TypeUnion.default }    => TypeUnion.default
    }

    given unliftTraceType: Unlifter[TraceType] with {
      def tpe = Type.of[TraceType]
      def unlift =
        case '{ TraceType.TypeCompute } => TraceType.TypeCompute
    }

    given unliftParams: Unlifter[Params] with {
      def tpe = Type.of[Params]
      def unlift =
        case '{ Params($collect, $verify, $typeUnion, $traceTypes) } =>
          Params(collect.fromExpr, verify.fromExpr, typeUnion.fromExpr, traceTypes.fromExpr)
        case '{ Params(($collect: Collect)) } =>
          Params(collect.fromExpr, Verify.default, TypeUnion.default, Nil)
        case '{ Params(($verify: Verify)) } =>
          Params(Collect.default, verify.fromExpr, TypeUnion.default, Nil)
        case '{ Params(($typeUnion: TypeUnion)) } =>
          Params(Collect.default, Verify.default, typeUnion.fromExpr, Nil)
        case '{ Params(($traceTypes: List[TraceType])) } =>
          Params(Collect.default, Verify.default, TypeUnion.default, traceTypes.fromExpr)
        case '{ Params.apply() } =>
          Params(Collect.default, Verify.default, TypeUnion.default, Nil)
    }
  }
}
