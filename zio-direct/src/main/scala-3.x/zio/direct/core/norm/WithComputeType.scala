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

  object ComputeIRT {
    private def applyCaseDef(ir: IR.Match.CaseDef)(implicit typeUnion: TypeUnion): IRT.Match.CaseDef =
      val rhsIRT = apply(ir.rhs)
      IRT.Match.CaseDef(ir.pattern, ir.guard, rhsIRT)(rhsIRT.zpe)

    // TODO should introduce IR/IRT.CaseDefs as a separate module that can hold a type
    //      so that this doesn't need to be recomputed
    def applyCaseDefs(caseDefs: List[IRT.Match.CaseDef])(implicit typeUnion: TypeUnion) =
      ZioType.composeN(caseDefs.map(_.zpe)).transformA(_.widen)

    def apply(ir: IR.Monadic)(implicit typeUnion: TypeUnion): IRT.Monadic =
      ir match
        case ir: IR.Fail     => apply(ir)
        case ir: IR.While    => apply(ir)
        case ir: IR.ValDef   => apply(ir)
        case ir: IR.Unsafe   => apply(ir)
        case ir: IR.Try      => apply(ir)
        case ir: IR.Foreach  => apply(ir)
        case ir: IR.FlatMap  => apply(ir)
        case ir: IR.Map      => apply(ir)
        case ir: IR.Monad    => apply(ir)
        case ir: IR.Block    => apply(ir)
        case ir: IR.Match    => apply(ir)
        case ir: IR.If       => apply(ir)
        case ir: IR.And      => apply(ir)
        case ir: IR.Or       => apply(ir)
        case ir: IR.Parallel => apply(ir)

    def apply(ir: IR.Leaf)(implicit typeUnion: TypeUnion): IRT.Leaf =
      ir match
        case ir: IR.Monad => apply(ir)
        case ir: IR.Pure  => apply(ir)

    def apply(ir: IR)(implicit typeUnion: TypeUnion): IRT =
      ir match
        case ir: IR.Monadic => apply(ir)
        case ir: IR.Leaf    => apply(ir)

    def apply(ir: IR.Monad)(implicit typeUnion: TypeUnion): IRT.Monad =
      IRT.Monad(ir.code, ir.source)(ZioType.fromZIO(ir.code))

    def apply(ir: IR.Pure)(implicit typeUnion: TypeUnion): IRT.Pure =
      IRT.Pure(ir.code)(ZioType.fromPure(ir.code))

    def apply(ir: IR.Fail)(implicit typeUnion: TypeUnion): IRT.Fail =
      ir match
        case IR.Fail(error) =>
          // The error type often is a single expression e.g. someFunctionThatThrowsError()
          // so we need to widen it into the underlying type
          val bodyError = apply(error)
          val tpe = ZioType(bodyError.zpe.r, bodyError.zpe.a.widenTermRefByName, TypeRepr.of[Nothing])
          IRT.Fail(bodyError)(tpe)

    def apply(ir: IR.While)(implicit typeUnion: TypeUnion): IRT.While =
      ir match
        case IR.While(cond, body) =>
          val condIRT = apply(cond)
          val bodyIRT = apply(body)
          val zpe = condIRT.zpe.flatMappedWith(bodyIRT.zpe).mappedWithType(TypeRepr.of[Unit])
          IRT.While(condIRT, bodyIRT)(zpe)

    def apply(ir: IR.ValDef)(implicit typeUnion: TypeUnion): IRT.ValDef =
      ir match
        case IR.ValDef(originalStmt, symbol, assignment, bodyUsingVal) =>
          // Eventually a ValDef will be turned into a flatMap so if we still have one,
          // the type-computation essentially is the same as the one for the flatMap
          //   val symbol = assignment
          //   bodyUsingVal
          // Basically becomes:
          //   assignment.flatMap { symbol =>
          //     bodyUsingVal
          //   }
          // Theis essentially means that `assignment` is the monad-equivalent
          // of flatMap and the bodyUsingVal is the body-equivalent.
          val assignmentIRT = apply(assignment)
          val bodyIRT = apply(bodyUsingVal)
          val zpe = assignmentIRT.zpe.flatMappedWith(bodyIRT.zpe)
          IRT.ValDef(ir.originalStmt, ir.symbol, assignmentIRT, bodyIRT)(zpe)

    def apply(ir: IR.Unsafe)(implicit typeUnion: TypeUnion): IRT.Unsafe =
      ir match
        case IR.Unsafe(body) =>
          // Things inside Unsafe blocks that are not inside runs will be wrapepd into ZIO.attempt
          // which will make their lower-bound Throwable in the MonadifyTries phase.
          // Do not need to do that manually here with the `e` type though.
          val unsafeIRT = apply(body)
          val zpe = unsafeIRT.zpe
          IRT.Unsafe(unsafeIRT)(zpe)

    def apply(ir: IR.Try)(implicit typeUnion: TypeUnion): IRT.Try =
      ir match
        case IR.Try(tryBlock, caseDefs, resultType, finallyBlock) =>
          val tryBlockIRT = apply(tryBlock)
          val caseDefIRTs = caseDefs.map(applyCaseDef(_))

          // We get better results by doing into the case-defs and computing the union-type of them
          // than we do by getting the information from outputType because outputType is limited
          // by Scala's ability to understand the try-catch. For example, if we infer from outputType,
          // the error-type will never be more concrete that `Throwable`.
          // (Note, widen the error type because even if it's a concrete int type
          // e.g. `catch { case e: Throwable => 111 }` we don't necessarily know
          // that this error will actually happen therefore it's not sensical to make it a singleton type)
          val caseDefType = applyCaseDefs(caseDefIRTs)

          // Could also apply from outputType but this has worse results, see comment above.
          // val caseDefType =
          //   outputType.asType match
          //     case '[ZIO[r, e, a]] => ZioType(TypeRepr.of[r].widen, TypeRepr.of[e].widen, TypeRepr.of[a].widen)
          //     case '[t]            => ZioType(TypeRepr.of[Any].widen, TypeRepr.of[Throwable].widen, TypeRepr.of[t].widen)
          val tpe = tryBlockIRT.zpe.flatMappedWith(caseDefType)
          val finallyBlockIRT = finallyBlock.map(apply(_))
          IRT.Try(tryBlockIRT, caseDefIRTs, ir.resultType, finallyBlockIRT)(tpe)

    def apply(ir: IR.Foreach)(implicit typeUnion: TypeUnion): IRT.Foreach =
      ir match
        case IR.Foreach(monad, _, _, body) =>
          val monadIRT = apply(monad)
          val bodyIRT = apply(body)
          val zpe = ZioType.fromUnitWithOthers(bodyIRT.zpe, monadIRT.zpe)
          IRT.Foreach(monadIRT, ir.listType, ir.elementSymbol, bodyIRT)(zpe)

    def apply(ir: IR.FlatMap)(implicit typeUnion: TypeUnion): IRT.FlatMap =
      ir match
        case IR.FlatMap(monad, valSymbol, body) =>
          val monadIRT = apply(monad)
          val bodyIRT = apply(body)
          val zpe = monadIRT.zpe.flatMappedWith(bodyIRT.zpe)
          IRT.FlatMap(monadIRT, ir.valSymbol, bodyIRT)(zpe)

    def apply(ir: IR.Map)(implicit typeUnion: TypeUnion): IRT.Map =
      ir match
        case IR.Map(monad, valSymbol, body @ IR.Pure(term)) =>
          val monadIRT = apply(monad)
          val zpe = monadIRT.zpe.mappedWith(term)
          IRT.Map(monadIRT, ir.valSymbol, IRT.Pure.fromTerm(term))(zpe)

    def apply(ir: IR.Block)(implicit typeUnion: TypeUnion): IRT.Block =
      ir match
        case IR.Block(head, tail) =>
          val tailIRT = apply(tail)
          IRT.Block(head, tailIRT)(tailIRT.zpe)

    def apply(ir: IR.Match)(implicit typeUnion: TypeUnion): IRT.Match =
      ir match
        case IR.Match(scrutinee, caseDefs) =>
          // Ultimately the scrutinee will be used if it is pure or lifted, either way we can
          // treat it as a value that will be flatMapped (or Mapped) against the caseDef values.
          val scrutineeIRT = apply(scrutinee)
          // NOTE: We can possibly do the same thing as IR.Try and pass through the Match result tpe, then
          // then use that to figure out what the type should be
          val caseDefIRTs = caseDefs.map(applyCaseDef(_))
          val caseDefType = ZioType.composeN(caseDefIRTs.map(_.zpe)).transformA(_.widen)
          val zpe = scrutineeIRT.zpe.flatMappedWith(caseDefType)
          IRT.Match(scrutineeIRT, caseDefIRTs)(zpe)

    def apply(ir: IR.Parallel)(implicit typeUnion: TypeUnion): IRT.Parallel =
      ir match
        case IR.Parallel(_, monadics, body) =>
          val monadicsIRTs =
            monadics.map((monadic, sym) => (apply(monadic), sym))

          // a IR.Leaf will either be a IR.Pure or an IR.Monad, both already have cases here
          val bodyIRT = apply(body).asInstanceOf[IRT.Leaf]

          // For some arbitrary structure that contains monads e.g:
          //   ((foo:ZIO[ConfA,ExA,A]).run, (bar:ZIO[ConfB,ExB,B]).run)
          // This will turn into something like:
          //   ZIO.collectAll(foo, bar).map(col => {val iter = col.iter; (iter.next, iter.next)}) which has the type
          // That means that the output signature will be a ZIO of the following:
          //   R-Parameter: ConfA & ConfB, E-Parameter: ExA | ExB, A-Parameter: (A, B)
          // In some cases the above function will be a flatMap and wrapped into a ZIO.attempt or ZIO.succeed
          //   so we include the body-type error and environment just in case
          val zpe =
            ZioType.fromPrimaryWithOthers(bodyIRT.zpe)(
              monadicsIRTs.map { case (mon, _) => mon.zpe }: _*
            )

          IRT.Parallel(ir.originalExpr, monadicsIRTs, bodyIRT)(zpe)

    def apply(ir: IR.If)(implicit typeUnion: TypeUnion): IRT.If =
      ir match
        case IR.If(cond, ifTrue, ifFalse) =>
          val condIRT = apply(cond)
          val ifTrueIRT = apply(ifTrue)
          val ifFalseIRT = apply(ifFalse)
          // Something like this: if (a) then b else c
          // Becomes something like this: a.flatMap(av => if (av) b.flatMap(...) else c.flatMap(...))
          // So the output type is the a.flatMappedWith(b compose c)
          val zpe =
            condIRT.zpe.flatMappedWith(
              ZioType.compose(ifTrueIRT.zpe, ifFalseIRT.zpe)
            )
          IRT.If(condIRT, ifTrueIRT, ifFalseIRT)(zpe)

    def apply(ir: IR.And)(implicit typeUnion: TypeUnion): IRT.And =
      ir match
        case IR.And(left, right) =>
          val leftIRT = apply(left)
          val rightIRT = apply(right)
          val zpe = ZioType.compose(leftIRT.zpe, rightIRT.zpe)
          IRT.And(leftIRT, rightIRT)(zpe)

    def apply(ir: IR.Or)(implicit typeUnion: TypeUnion): IRT.Or =
      ir match
        case IR.Or(left, right) =>
          val leftIRT = apply(left)
          val rightIRT = apply(right)
          val zpe = ZioType.compose(leftIRT.zpe, rightIRT.zpe)
          IRT.Or(leftIRT, rightIRT)(zpe)
  }
}
