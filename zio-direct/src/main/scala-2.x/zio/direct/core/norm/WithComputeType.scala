package zio.direct.core.norm

import zio.direct.core.metaprog.WithIR
import zio.direct.core.metaprog.WithPrintIR
import zio.direct.core.metaprog.WithZioType
import zio.direct.core.util.WithInterpolator
import zio.direct.core.util.WithFormat
import zio.direct.core.metaprog.Instructions
import zio.direct.core.metaprog.TypeUnion
import zio.direct.core.metaprog.MacroBase

trait WithComputeType extends MacroBase {
  self: WithIR with WithZioType with WithInterpolator with WithPrintIR with WithFormat =>

  import c.universe._

  object ComputeType {
    def fromIR(ir: IR)(implicit instructions: Instructions): IRT =
      new ComputeType(instructions).apply(ir)
  }

  class ComputeType private (instructions: Instructions) {
    implicit val tu: TypeUnion = instructions.typeUnion

    def apply(ir: IR): IRT =
      ir match {
        case ir: IR.Leaf => handleLeaf(ir)
        case ir: IR.Monadic => handleMonadic(ir)
      }

    def handleLeaf(ir: IR.Leaf): IRT.Leaf =
      ir match {
        case ir: IR.Pure => handlePure(ir)
        case ir: IR.Monad => handleMonad(ir)
      }


    def handleMonadic(ir: IR.Monadic): IRT.Monadic =
      ir match {
        case ir: IR.Monad => handleMonad(ir)
        case ir: IR.FlatMap => handleFlatMap(ir)
        case ir: IR.ValDef => handleValDef(ir)
        case ir: IR.Map => handleMap(ir)
        case ir: IR.Block => handleBlock(ir)
        case ir: IR.Fail => handleFail(ir)
        case ir: IR.Unsafe => handleUnsafe(ir)
        case ir: IR.Match => handleMatch(ir)
        case ir: IR.If => handleIf(ir)
        case ir: IR.And => handleAnd(ir)
        case ir: IR.Or => handleOr(ir)
        case ir: IR.Try => handleTry(ir)
        case ir: IR.While => handleWhile(ir)
        case ir: IR.Foreach => handleForeach(ir)
        case ir: IR.Parallel => handleParallel(ir)
      }

    private def handleParallel(ir: IR.Parallel) = {
      val monadTs =
        ir.monads.map { case (monadic, termName, tpe) => (handleMonadic(monadic), termName, tpe) }

      // a IR.Leaf will either be a IR.Pure or an IR.Monad, both already have cases here
      val bodyT = handleLeaf(ir.body)

      // For some arbitrary structure that contains monads e.g:
      //   ((foo:ZIO[ConfA,ExA,A]).run, (bar:ZIO[ConfB,ExB,B]).run)
      // This will turn into something like:
      //   ZIO.collectAll(foo, bar).map(col => {val iter = col.iter; (iter.next, iter.next)}) which has the type
      // That means that the output signature will be a ZIO of the following:
      //   R-Parameter: ConfA & ConfB, E-Parameter: ExA | ExB, A-Parameter: (A, B)
      // In some cases the above function will be a flatMap and wrapped into a ZIO.attempt or ZIO.succeed
      //   so we include the body-type error and environment just in case
      val zpe = ZioType.fromPrimaryWithOthers(bodyT.zpe)(monadTs.map(_._1.zpe): _*)(instructions.typeUnion)
      IRT.Parallel(ir.originalExpr, monadTs, bodyT)(zpe)
    }

    // The error type often is a single expression e.g. someFunctionThatThrowsError()
    // so we need to widen it into the underlying type
    private def handleFail(ir: IR.Fail) = {
      val bodyError = apply(ir.error)
      val bodyZpe = bodyError.zpe
      val zpe = ZioType(bodyZpe.r, bodyZpe.a.widen, typeOf[Nothing])
      IRT.Fail(bodyError)(zpe)
    }

    private def handlePure(ir: IR.Pure): IRT.Pure = IRT.Pure(ir.code)(ZioType.fromPure(ir.code))

    private def handleMonad(ir: IR.Monad): IRT.Monad = IRT.Monad(ir.code)(ZioType.fromZIO(ir.code))

    private def handleFlatMap(ir: IR.FlatMap): IRT.FlatMap = {
      val monadT = handleMonadic(ir.monad)
      val bodyT = handleMonadic(ir.body)
      val zpe = monadT.zpe.flatMappedWith(bodyT.zpe)
      IRT.FlatMap(monadT, ir.valSymbol, bodyT)(zpe)
    }

    private def handleBlock(ir: IR.Block): IRT.Block = {
      val tailT = handleMonadic(ir.tail)
      IRT.Block(ir.head, tailT)(tailT.zpe)
    }

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
    private def handleValDef(ir: IR.ValDef) = {
      val assignmentT = apply(ir.assignment)
      val bodyT = apply(ir.bodyUsingVal)
      val zpe = assignmentT.zpe.flatMappedWith(bodyT.zpe)
      IRT.ValDef(ir.originalStmt, ir.symbol, assignmentT, bodyT)(zpe)
    }

    private def handleMap(ir: IR.Map): IRT.Map = {
      val monadT = handleMonadic(ir.monad)
      val bodyT = handlePure(ir.body)
      val zpe = monadT.zpe.mappedWith(ir.body.code)
      IRT.Map(monadT, ir.valSymbol, bodyT)(zpe)
    }

    // Things inside Unsafe blocks that are not inside runs will be wrapepd into ZIO.attempt
    // which will make their lower-bound Throwable in the MonadifyTries phase.
    // Do not need to do that manually here with the `e` type though.
    private def handleUnsafe(ir: IR.Unsafe): IRT.Unsafe = {
      val bodyT = apply(ir.body)
      IRT.Unsafe(bodyT)(bodyT.zpe)
    }

    private def handleMatch(ir: IR.Match) = {
      // Ultimately the scrutinee will be used if it is pure or lifted, either way we can
      // treat it as a value that will be flatMapped (or Mapped) against the caseDef values.
      val scrutineeT = apply(ir.scrutinee)
      // NOTE: We can possibly do the same thing as IR.Try and pass through the Match result tpe, then
      // then use that to figure out what the type should be
      val caseDefTs = ir.caseDefs.map(caseDef => handleCaseDef(caseDef))
      val caseDefTotalType = ZioType.composeN(caseDefTs.map(_.zpe))(instructions.typeUnion)
      val zpe = scrutineeT.zpe.flatMappedWith(caseDefTotalType)
      IRT.Match(scrutineeT, caseDefTs)(zpe)
    }

    private def handleCaseDef(ir: IR.Match.CaseDef) = {
      val rhsT = handleMonadic(ir.rhs)
      IRT.Match.CaseDef(ir.pattern, ir.guard, rhsT)(rhsT.zpe)
    }

    private def handleIf(ir: IR.If) = {
      val condT = apply(ir.cond)
      val ifTrueT = apply(ir.ifTrue)
      val ifFalseT = apply(ir.ifFalse)
      val expressionType = ZioType.compose(ifTrueT.zpe, ifFalseT.zpe)
      val zpe = condT.zpe.flatMappedWith(expressionType)
      IRT.If(condT, ifTrueT, ifFalseT)(zpe)
    }

    private def handleAnd(ir: IR.And) = {
      val leftT = apply(ir.left)
      val rightT = apply(ir.right)
      val zpe = ZioType.compose(leftT.zpe, rightT.zpe)
      IRT.And(leftT, rightT)(zpe)
    }

    private def handleOr(ir: IR.Or) = {
      val leftT = apply(ir.left)
      val rightT = apply(ir.right)
      val zpe = ZioType.compose(leftT.zpe, rightT.zpe)
      IRT.Or(leftT, rightT)(zpe)
    }

    private def handleTry(ir: IR.Try) = {
      val tryBlockT = apply(ir.tryBlock)
      val caseTs = ir.cases.map(handleCaseDef)
      val finallyT = ir.finallyBlock.map(apply)
      // We get better results by doing into the case-defs and computing the union-type of them
      // than we do by getting the information from outputType because outputType is limited
      // by Scala's ability to understand the try-catch. For example, if we infer from outputType,
      // the error-type will never be more concrete that `Throwable`.
      // (Note, widen the error type because even if it's a concrete int type
      // e.g. `catch { case e: Throwable => 111 }` we don't necessarily know
      // that this error will actually happen therefore it's not sensical to make it a singleton type)
      val caseDefType =
      ZioType.composeN(caseTs.map(_.zpe))(instructions.typeUnion).transformA(_.widen)
      // Could also compute from outputType but this has worse results, see comment above.
      // val caseDefType =
      //   outputType.asType match
      //     case '[ZIO[r, e, a]] => ZioType(TypeRepr.of[r].widen, TypeRepr.of[e].widen, TypeRepr.of[a].widen)
      //     case '[t]            => ZioType(TypeRepr.of[Any].widen, TypeRepr.of[Throwable].widen, TypeRepr.of[t].widen)
      val zpe = tryBlockT.zpe.flatMappedWith(caseDefType)
      IRT.Try(tryBlockT, caseTs, ir.resultType, finallyT)(zpe)
    }

    private def handleWhile(ir: IR.While) = {
      val condT = apply(ir.cond)
      val bodyT = apply(ir.body)
      val zpe = condT.zpe.flatMappedWith(bodyT.zpe).mappedWithType(typeOf[Unit])
      IRT.While(condT, bodyT)(zpe)
    }

    private def handleForeach(ir: IR.Foreach) = {
      val listT = apply(ir.list)
      val bodyT = apply(ir.body)
      val zpe = ZioType.fromUnitWithOthers(bodyT.zpe, listT.zpe)(instructions.typeUnion)
      IRT.Foreach(listT, ir.listType, ir.elementSymbol, bodyT)(zpe)
    }
  }
}
