package zio.direct

import zio.Task
import scala.quoted._
import zio.direct.core.Transformer
import zio.ZIO
import zio.direct.core.metaprog.Instructions
import zio.direct.core.metaprog.InfoBehavior
import zio.direct.core.metaprog.Collect
import zio.direct.core.metaprog.Unliftables
import zio.direct.core.metaprog.Verify
import zio.direct.core.NotDeferredException

def unsafe[T](value: T): T = NotDeferredException.fromNamed("unsafe")

object defer {
  transparent inline def apply[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Silent }, '{ Dsl.Params() }) }
  transparent inline def info[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Info }, '{ Dsl.Params() }) }
  transparent inline def verbose[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Verbose }, '{ Dsl.Params() }) }
  transparent inline def verboseTree[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.VerboseTree }, '{ Dsl.Params() }) }

  transparent inline def apply[T](inline params: Dsl.Params)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Silent }, 'params) }
  transparent inline def info[T](inline params: Dsl.Params)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Info }, 'params) }
  transparent inline def verbose[T](inline params: Dsl.Params)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Verbose }, 'params) }
  transparent inline def verboseTree[T](inline params: Dsl.Params)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.VerboseTree }, 'params) }
}

extension [R, E, A](value: ZIO[R, E, A]) {
  def run: A = NotDeferredException.fromNamed("run")
}

object Dsl {
  import InfoBehavior._
  class Params private (val collect: Collect, val verify: Verify)
  object Params {
    def apply(collect: Collect, verify: Verify) =
      new Params(collect, verify)
    def apply(collect: Collect) =
      new Params(collect, Verify.Strict)
    def apply(verify: Verify) =
      new Params(Collect.Sequence, verify)
    def apply() =
      new Params(Collect.Sequence, Verify.Strict)
  }

  def impl[T: Type](value: Expr[T], infoExpr: Expr[InfoBehavior], paramsExpr: Expr[Params])(using q: Quotes): Expr[ZIO[?, ?, ?]] =
    import quotes.reflect._
    val infoBehavior = Unliftables.unliftInfoBehavior(infoExpr.asTerm.underlyingArgument.asExprOf[InfoBehavior])
    val params = Unliftables.unliftParams(paramsExpr.asTerm.underlyingArgument.asExprOf[Params])
    doTransform(value, Instructions(infoBehavior, params.collect, params.verify))

  def doTransform[T: Type](value: Expr[T], instructions: Instructions)(using q: Quotes): Expr[ZIO[?, ?, ?]] =
    (new Transformer(q)).apply(value, instructions)
}
