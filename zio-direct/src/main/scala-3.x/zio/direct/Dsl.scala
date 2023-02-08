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
import zio.direct.core.metaprog.RefineInstructions
import zio.direct.core.metaprog.Linearity

class directRunCall extends scala.annotation.StaticAnnotation
class directSetCall extends scala.annotation.StaticAnnotation
class directGetCall extends scala.annotation.StaticAnnotation
class directLogCall extends scala.annotation.StaticAnnotation

def unsafe[T](value: T): T = NotDeferredException.fromNamed("unsafe")

trait deferCall[F[_, _, _], F_out, S, W] {
  transparent inline def impl[T](inline value: T, inline info: InfoBehavior, inline use: Use, inline linearity: Linearity) = ${ zio.direct.Dsl.impl[T, F, F_out, S, W]('value, 'info, 'use, 'linearity) }

  import zio.direct.core.metaprog.Linearity.{Regular => LinReg, Linear => Lin}

  // @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def apply[T](inline value: T): F_out = impl(value, InfoBehavior.Silent, Use, LinReg)
  // @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def info[T](inline value: T): F_out = impl(value, InfoBehavior.Info, Use, LinReg)
  // @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def verbose[T](inline value: T): F_out = impl(value, InfoBehavior.Verbose, Use, LinReg)
  // @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def verboseTree[T](inline value: T): F_out = impl(value, InfoBehavior.VerboseTree, Use, LinReg)

  // @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def apply[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.Silent, params, LinReg)
  // @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def info[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.Info, params, LinReg)
  // @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def verbose[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.Verbose, params, LinReg)
  // @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def verboseTree[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.VerboseTree, params, LinReg)

  object linear {
    // @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def apply[T](inline value: T): F_out = impl(value, InfoBehavior.Silent, Use, Lin)
    // @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def info[T](inline value: T): F_out = impl(value, InfoBehavior.Info, Use, Lin)
    // @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def verbose[T](inline value: T): F_out = impl(value, InfoBehavior.Verbose, Use, Lin)
    // @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def verboseTree[T](inline value: T): F_out = impl(value, InfoBehavior.VerboseTree, Use, Lin)

    // @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def apply[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.Silent, params, Lin)
    // @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def info[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.Info, params, Lin)
    // @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def verbose[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.Verbose, params, Lin)
    // @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def verboseTree[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.VerboseTree, params, Lin)
  }
}

object defer extends deferCall[ZIO, ZIO[?, ?, ?], Nothing, Nothing]

extension [R, E, A](value: ZIO[R, E, A]) {
  @directRunCall
  def run: A = NotDeferredException.fromNamed("run")
}

object Dsl {
  import InfoBehavior._

  // def implZIO[T: Type](value: Expr[T], infoExpr: Expr[InfoBehavior], useTree: Expr[Use])(using q: Quotes) =
  //   impl[T, ZIO, ZIO[?, ?, ?]](value, infoExpr, useTree)

  def impl[T: Type, F[_, _, _]: Type, F_out: Type, S: Type, W: Type](value: Expr[T], infoExpr: Expr[InfoBehavior], useTree: Expr[Use], linearityExpr: Expr[Linearity])(using q: Quotes): Expr[F_out] =
    import quotes.reflect._
    val linearity = Unliftables.unliftLinearity(linearityExpr.asTerm.underlyingArgument.asExprOf[Linearity])
    val infoBehavior = Unliftables.unliftInfoBehavior(infoExpr.asTerm.underlyingArgument.asExprOf[InfoBehavior])
    val instructionsRaw =
      Instructions.default.copy(
        info = infoBehavior,
        linearity = linearity,
        verify = {
          linearity match
            case Linearity.Regular => Instructions.default.verify
            // since linear mode disables the IR.Parallel situation, we are safe to do lenient mode
            case Linearity.Linear => Verify.Lenient
        }
      )
    val instructions = RefineInstructions.fromUseTree(useTree, instructionsRaw)
    (new Transformer[F, F_out, S, W](q)).apply(value, instructions)
    // (new Transformer[ZIO, ZIO[?, ?, ?]](q)).apply(value, instructions)

}

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
  // def deferred[F[_, _, _], A, B, C](effect: F[A, B, C]) = effect
  // def deferred[A, B, C](effect: ZIO[A, B, C]) = effect
  def deferred[T](effect: T): T = effect

  def ignore[T](code: T): T =
    throw new NotDeferredException(s"The construct `ignore` be used inside of a `defer { ... }` block and should only be used for testing purposes!")

  // Artificial markers use to deconstruct types in the Transformer. Not necessary otherwise.
  object Marker {
    type A
    type B
    type C
  }
}
