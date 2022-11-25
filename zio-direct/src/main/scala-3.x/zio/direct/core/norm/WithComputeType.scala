package zio.direct.core.norm

import zio.direct.core.metaprog.WithIR
import scala.quoted._
import zio.direct.core.util.Format
import zio.ZIO
import zio.direct.core.metaprog.WithPrintIR
import zio.direct.core.metaprog.WithZioType
import zio.direct.core.util.WithInterpolator
import zio.direct.core.util.TraceType
import zio.direct.core.metaprog.Instructions
import zio.direct.core.metaprog.Embedder.topLevelOwner
import zio.direct.core.metaprog.TypeUnion
import zio.direct.core.metaprog.Embedder

trait WithComputeType {
  self: WithIR with WithZioType with WithInterpolator with WithPrintIR =>

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  object ComputeType {
    def fromIR(ir: IR)(implicit instructions: Instructions): ZioType =
      new ComputeType(instructions).apply(ir)
  }

  class ComputeType private (instructions: Instructions) {
    implicit val tu: TypeUnion = instructions.typeUnion

    def apply(ir: IR): ZioType =
      ir match {
        case ir @ IR.Pure(code) =>
          ZioType.fromPure(code)

        case ir @ IR.FlatMap(monad, valSymbol, body) =>
          apply(monad).flatMappedWith(apply(body))

        case ir @ IR.Map(monad, valSymbol, IR.Pure(term)) =>
          apply(monad).mappedWith(term)

        case ir @ IR.Monad(code) =>
          ZioType.fromZIO(code)

        case ir @ IR.Block(head, tail) =>
          apply(tail)

        case ir @ IR.Fail(error) =>
          // The error type often is a single expression e.g. someFunctionThatThrowsError()
          // so we need to widen it into the underlying type
          val bodyError = apply(error)
          ZioType(bodyError.r, bodyError.a.widenTermRefByName, TypeRepr.of[Nothing])

        // Things inside Unsafe blocks that are not inside runs will be wrapepd into ZIO.attempt
        // which will make their lower-bound Throwable in the MonadifyTries phase.
        // Do not need to do that manually here with the `e` type though.
        case ir @ IR.Unsafe(body) =>
          apply(body)

        case ir @ IR.Match(scrutinee, caseDefs) =>
          // Ultimately the scrutinee will be used if it is pure or lifted, either way we can
          // treat it as a value that will be flatMapped (or Mapped) against the caseDef values.
          val scrutineeType = apply(scrutinee)
          // NOTE: We can possibly do the same thing as IR.Try and pass through the Match result tpe, then
          // then use that to figure out what the type should be
          val caseDefTypes = caseDefs.map(caseDef => apply(caseDef.rhs))
          val caseDefTotalType = ZioType.composeN(caseDefTypes)(instructions.typeUnion)
          scrutineeType.flatMappedWith(caseDefTotalType)

        case ir @ IR.If(cond, ifTrue, ifFalse) =>
          val condType = apply(cond)
          val expressionType = ZioType.compose(apply(ifTrue), apply(ifFalse))
          condType.flatMappedWith(expressionType)

        case ir @ IR.And(left, right) =>
          ZioType.compose(apply(left), apply(right))

        case ir @ IR.Or(left, right) =>
          ZioType.compose(apply(left), apply(right))

        case ir @ IR.Try(tryBlock, caseDefs, outputType, finallyBlock) =>
          val tryBlockType = apply(tryBlock)
          // We get better results by doing into the case-defs and computing the union-type of them
          // than we do by getting the information from outputType because outputType is limited
          // by Scala's ability to understand the try-catch. For example, if we infer from outputType,
          // the error-type will never be more concrete that `Throwable`.
          val caseDefType =
            ZioType.composeN(caseDefs.map(caseDef => apply(caseDef.rhs)))(instructions.typeUnion)
          // Could also compute from outputType but this has worse results, see comment above.
          // val caseDefType =
          //   outputType.asType match
          //     case '[ZIO[r, e, a]] => ZioType(TypeRepr.of[r].widen, TypeRepr.of[e].widen, TypeRepr.of[a].widen)
          //     case '[t]            => ZioType(TypeRepr.of[Any].widen, TypeRepr.of[Throwable].widen, TypeRepr.of[t].widen)
          tryBlockType.flatMappedWith(caseDefType)

        case ir @ IR.While(cond, body) =>
          val condTpe = apply(cond)
          val bodyTpe = apply(body)
          condTpe.flatMappedWith(bodyTpe).mappedWithType(TypeRepr.of[Unit])

        case ir @ IR.Foreach(monad, _, _, body) =>
          val monadType = apply(monad)
          val bodyType = apply(body)
          ZioType(
            ZioType.and(bodyType.r, monadType.r),
            ZioType.or(bodyType.e, monadType.e)(instructions.typeUnion),
            TypeRepr.of[Unit]
          )

        case ir @ IR.Parallel(monadics, body) =>
          val monadTypes =
            monadics.map((monadic, _) => apply(monadic))

          // a IR.Leaf will either be a IR.Pure or an IR.Monad, both already have cases here
          val bodyType = apply(body)

          // For some arbitrary structure that contains monads e.g:
          //   ((foo:ZIO[ConfA,ExA,A]).run, (bar:ZIO[ConfB,ExB,B]).run)
          // This will turn into something like:
          //   ZIO.collectAll(foo, bar).map(col => {val iter = col.iter; (iter.next, iter.next)}) which has the type
          // That means that the output signature will be a ZIO of the following:
          //   R-Parameter: ConfA & ConfB, E-Parameter: ExA | ExB, A-Parameter: (A, B)
          // In some cases the above function will be a flatMap and wrapped into a ZIO.attempt or ZIO.succeed
          //   so we include the body-type error and environment just in case
          ZioType(
            ZioType.andN(bodyType.r +: monadTypes.map(_.r)),
            ZioType.orN(bodyType.e +: monadTypes.map(_.e))(instructions.typeUnion),
            bodyType.a
          )
      }
  }
}
