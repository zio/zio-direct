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
import zio.direct.core.util.TraceType
import zio.direct.core.metaprog.TypeUnion

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
  class Params private (val collect: Collect, val verify: Verify, val typeUnion: TypeUnion, val traceTypes: List[TraceType])
  object Params {
    def apply(collect: Collect, verify: Verify, typeUnion: TypeUnion, traceTypes: List[TraceType]) =
      new Params(collect, verify, typeUnion, traceTypes)
    def apply(collect: Collect) =
      new Params(collect, Verify.Strict, TypeUnion.OrType, Nil)
    def apply(verify: Verify) =
      new Params(Collect.Sequence, verify, TypeUnion.OrType, Nil)
    def apply(traceTypes: List[TraceType]) =
      new Params(Collect.Sequence, Verify.Strict, TypeUnion.OrType, traceTypes)
    def apply(typeUnion: TypeUnion) =
      new Params(Collect.Sequence, Verify.Strict, typeUnion, Nil)
    def apply() =
      new Params(Collect.Sequence, Verify.Strict, TypeUnion.OrType, Nil)
  }

  def impl[T: Type](value: Expr[T], infoExpr: Expr[InfoBehavior], paramsExpr: Expr[Params])(using q: Quotes): Expr[ZIO[?, ?, ?]] =
    import quotes.reflect._
    val infoBehavior = Unliftables.unliftInfoBehavior(infoExpr.asTerm.underlyingArgument.asExprOf[InfoBehavior])
    val params = Unliftables.unliftParams(paramsExpr.asTerm.underlyingArgument.asExprOf[Params])
    doTransform(value, Instructions(infoBehavior, params.collect, params.verify, params.typeUnion, params.traceTypes))

  def doTransform[T: Type](value: Expr[T], instructions: Instructions)(using q: Quotes): Expr[ZIO[?, ?, ?]] =
    (new Transformer(q)).apply(value, instructions)

  object Internal {
    // An indicator to mark which parts of code have already been transformed by the defer macro
    // this is so that in cases where you have `defer { defer(code) }` the inner defer will yield
    // something like `defer { deferred(code) }` and the outer one knows to wrap the entire
    // body of the inner code into an IR.Monad block
    // (Note that in cases where the `defer(code)` is in some previous operation in the program
    // e.g. `val x = defer {code}; defer {x}` the `defer(code)` information will be lost again but we
    // don't care about it because all the macro-engine sees at that point is an identifier i.e.
    // `defer { Ident(x) }` (unless it's a Scala 3 inline-def that propagates the tree).
    //  This is an important note to understand about metaprogramming in general,
    // you have to not be able to care about the structure of things transformed in other macros
    // in other parts of the application.)
    def deferred[R, E, A](effect: ZIO[R, E, A]) = effect

    def ignore[T](code: T): T =
      throw new NotDeferredException(s"The construct `ignore` be used inside of a `defer { ... }` block and should only be used for testing purposes!")
  }
}
