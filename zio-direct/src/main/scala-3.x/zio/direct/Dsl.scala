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

/**
 * Runs a block as plain code inside a `defer { ... }` block, not as effects. Use it when the block calls
 * code that throws. An exception thrown inside `unsafe` becomes a failure of the effect (the error channel,
 * as a `Throwable`) instead of a defect. Calls to `run` inside `unsafe` still run as effects, in order.
 * A line inside `unsafe` that mixes `run` with other expressions is rejected, as in `defer.linear`.
 *
 * Outside a `defer` block, `unsafe` throws `NotDeferredException`.
 *
 * {{{
 * import zio._
 * import zio.direct._
 *
 * def parse(s: String): Int = s.toInt
 *
 * val program: ZIO[Any, Throwable, Int] = defer {
 *   unsafe {
 *     val text = ZIO.succeed("42").run
 *     parse(text)
 *   }
 * }
 * }}}
 */
def unsafe[T](value: T): T = NotDeferredException.fromNamed("unsafe")

/**
 * The shared body of each `defer` object (ZIO, streams, futures, pure and lists). Each object extends
 * this trait and fills in the monad hooks below. The hooks tell the macro how that monad wraps values,
 * chains effects, fails, sequences, and handles state and logs.
 *
 * Callers use the methods of the object, such as `defer { ... }` or `defer.verbose { ... }`. Inside the
 * block, a `run` call (or the monad's own equivalent, such as `each`) runs an effect and gives its value.
 *
 * {{{
 * import zio._
 * import zio.direct._
 *
 * val program: ZIO[Any, Nothing, Int] = defer {
 *   val a = ZIO.succeed(1).run
 *   val b = ZIO.succeed(2).run
 *   a + b
 * }
 * }}}
 */
trait deferCall[F[_, _, _], F_out, S, W, MM <: MonadModel] {

  /**
   * Hook for the monad's basic operations: wrapping a plain value, `map` and `flatMap`. Each DSL object sets it.
   */
  transparent inline def success: MonadSuccess[F]

  /**
   * Hook for the monad's error operations (`fail`, `attempt`, `catchSome`, `mapError`, `orDie`, `ensuring`).
   * It is `None` when the monad has no error channel, as for lists.
   */
  transparent inline def fallible: Option[MonadFallible[F]]

  /**
   * Hook for running a collection of effects one after another (`foreach` and `collectAll`).
   */
  transparent inline def sequence: MonadSequence[F]

  /**
   * Hook used in place of `sequence` (`foreachPar` and `collectAllPar`) when `Use.withParallelEval` is set. For
   * ZIO it runs the effects in parallel. The streams, futures, pure and list DSLs run them one after another.
   */
  transparent inline def sequencePar: MonadSequenceParallel[F]

  /**
   * Hook for reading and setting state. It is `None` unless the monad has state. Only `deferWith` sets it.
   */
  transparent inline def state: Option[MonadState[F, S]]

  /**
   * Hook for writing log entries. It is `None` unless the monad has a log. Only `deferWith` sets it.
   */
  transparent inline def log: Option[MonadLog[F, W]]

  /**
   * Internal. The macro behind the methods below. Call `apply`, `info`, `verbose` or `verboseTree` instead.
   */
  transparent inline def impl[T](
      inline value: T,
      inline info: InfoBehavior,
      inline use: Use,
      inline linearity: Linearity
  ) = ${ zio.direct.Dsl.impl[T, F, F_out, S, W, MM]('value, 'info, 'use, 'linearity, 'success, 'fallible, 'sequence, 'sequencePar, 'state, 'log) }

  import zio.direct.core.metaprog.Linearity.{Regular => LinReg, Linear => Lin}

  /**
   * Turns `value` into the monad's type. Each `run` call inside the block becomes a step of the result.
   * Uses the default settings: strict checks (no `var`), one-after-another tuples, and a union of error types.
   */
  @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def apply[T](inline value: T): F_out = impl(value, InfoBehavior.Silent, Use, LinReg)

  /**
   * Same as `apply`, and at compile time prints the computed type of the block and the code generated for it.
   * The output does not change the program.
   */
  @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def info[T](inline value: T): F_out = impl(value, InfoBehavior.Info, Use, LinReg)

  /**
   * Same as `info`, with more compile-time output. It also prints the intermediate steps of the transformation.
   */
  @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def verbose[T](inline value: T): F_out = impl(value, InfoBehavior.Verbose, Use, LinReg)

  /**
   * Same as `verbose`, and also prints the generated code as a tree.
   */
  @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def verboseTree[T](inline value: T): F_out = impl(value, InfoBehavior.VerboseTree, Use, LinReg)

  /**
   * Same as `apply(value)`, with the settings in `params`, for example `defer(Use.withLenientCheck) { ... }`.
   * See `Use`.
   */
  @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def apply[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.Silent, params, LinReg)

  /**
   * Same as `info(value)`, with the settings in `params`. See `Use`.
   */
  @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def info[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.Info, params, LinReg)

  /**
   * Same as `verbose(value)`, with the settings in `params`. See `Use`.
   */
  @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def verbose[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.Verbose, params, LinReg)

  /**
   * Same as `verboseTree(value)`, with the settings in `params`. See `Use`.
   */
  @scala.reflect.macros.internal.macroImpl("nothing")
  transparent inline def verboseTree[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.VerboseTree, params, LinReg)

  /**
   * Linear mode. Local `var`s are allowed, because the checks are lenient. Each line must be one step: a line
   * that mixes `run` with other expressions, such as the tuple `(a.run, b.run)`, is rejected. Split such a
   * line into two `val`s, or use `defer` instead.
   *
   * {{{
   * import zio._
   * import zio.direct._
   *
   * val program: ZIO[Any, Nothing, Int] = defer.linear {
   *   var total = 0
   *   total = total + 3
   *   total
   * }
   * }}}
   *
   * The methods are the same as on `defer`, run in linear mode.
   */
  object linear {

    /**
     * Same as `defer.apply`, in linear mode.
     */
    @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def apply[T](inline value: T): F_out = impl(value, InfoBehavior.Silent, Use, Lin)

    /**
     * Same as `defer.info`, in linear mode.
     */
    @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def info[T](inline value: T): F_out = impl(value, InfoBehavior.Info, Use, Lin)

    /**
     * Same as `defer.verbose`, in linear mode.
     */
    @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def verbose[T](inline value: T): F_out = impl(value, InfoBehavior.Verbose, Use, Lin)

    /**
     * Same as `defer.verboseTree`, in linear mode.
     */
    @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def verboseTree[T](inline value: T): F_out = impl(value, InfoBehavior.VerboseTree, Use, Lin)

    /**
     * Same as `defer.apply(params)(value)`, in linear mode.
     */
    @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def apply[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.Silent, params, Lin)

    /**
     * Same as `defer.info(params)(value)`, in linear mode.
     */
    @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def info[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.Info, params, Lin)

    /**
     * Same as `defer.verbose(params)(value)`, in linear mode.
     */
    @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def verbose[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.Verbose, params, Lin)

    /**
     * Same as `defer.verboseTree(params)(value)`, in linear mode.
     */
    @scala.reflect.macros.internal.macroImpl("nothing")
    transparent inline def verboseTree[T](inline params: Use)(inline value: T): F_out = impl(value, InfoBehavior.VerboseTree, params, Lin)
  }
}

extension [R, E, A](value: ZIO[R, E, A]) {

  /**
   * Runs this effect inside a `defer { ... }` block and gives its value. Outside a `defer` block it throws
   * `NotDeferredException`.
   */
  @directRunCall
  def run: A = NotDeferredException.fromNamed("run")
}

object Dsl {
  import InfoBehavior._

  case class DirectMonadInput[F[_, _, _]: Type, S: Type, W: Type](
      success: Expr[MonadSuccess[F]],
      fallible: Expr[Option[MonadFallible[F]]],
      sequence: Expr[MonadSequence[F]],
      sequencePar: Expr[MonadSequenceParallel[F]],
      state: Expr[Option[MonadState[F, S]]],
      log: Expr[Option[MonadLog[F, W]]]
  )

  def impl[T: Type, F[_, _, _]: Type, F_out: Type, S: Type, W: Type, MM <: MonadModel: Type](
      value: Expr[T],
      infoExpr: Expr[InfoBehavior],
      useTree: Expr[Use],
      linearityExpr: Expr[Linearity],
      success: Expr[MonadSuccess[F]],
      fallible: Expr[Option[MonadFallible[F]]],
      sequence: Expr[MonadSequence[F]],
      sequencePar: Expr[MonadSequenceParallel[F]],
      state: Expr[Option[MonadState[F, S]]],
      log: Expr[Option[MonadLog[F, W]]]
  )(using q: Quotes): Expr[F_out] =
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

    val input = DirectMonadInput(success, fallible, sequence, sequencePar, state, log)

    val instructions = RefineInstructions.fromUseTree(useTree, instructionsRaw)
    (new Transformer[F, F_out, S, W, MM](q)).apply(value, instructions, input)
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
