package zio.direct.core.norm

import zio.direct.core.metaprog.WithIR
import scala.quoted._
import zio.direct.core.util.Format
import zio.ZIO

trait WithComputeType {
  self: WithIR =>

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  protected case class ZioType(r: TypeRepr, e: TypeRepr, a: TypeRepr) {
    def show = s"ZioType(${Format.TypeRepr(r)}, ${Format.TypeRepr(e)}, ${Format.TypeRepr(a)})"

    def asTypeTuple = (r.asType, e.asType, a.asType)

    def toZioType: TypeRepr =
      asTypeTuple match
        case ('[r], '[e], '[a]) =>
          TypeRepr.of[ZIO[r, e, a]]

    def flatMappedWith(other: ZioType) =
      ZioType(ZioType.and(r, other.r), ZioType.or(e, other.e), other.a)

    def mappedWith(other: Term) =
      ZioType(r, e, other.tpe)

    def mappedWithType(tpe: TypeRepr) =
      ZioType(r, e, tpe)
  }
  protected object ZioType {
    def fromZIO(zio: Term) =
      zio.tpe.asType match
        case '[ZIO[r, e, a]] =>
          ZioType(TypeRepr.of[r], TypeRepr.of[e], TypeRepr.of[a])
        case _ =>
          report.errorAndAbort(s"The type of ${Format.Term(zio)} is not a ZIO. It is: ${Format.TypeRepr(zio.tpe)}")

    // In this case the error is considered to be Nothing (since we are not wrapping error handling for pure values)
    // and the environment type is considered to be Any (it will be removed via later `ZioType.union` calls if it can be specialized).
    // Only the output type is used
    def fromPure(term: Term) =
      ZioType(TypeRepr.of[Any], TypeRepr.of[Nothing], term.tpe)

    def composeN(zioTypes: List[ZioType]): ZioType =
      val (rs, es, as) = zioTypes.map(zt => (zt.r, zt.e, zt.a)).unzip3
      val out = ZioType(andN(rs), orN(es), andN(as))
      // println(s"composeN Inputs: ${zioTypes.map(_.show)}. Output: ${out.show}")
      out

    def andN(types: List[TypeRepr]) =
      if (types.length > 0)
        types.reduce(and(_, _))
      else
        TypeRepr.of[Any]

    def orN(types: List[TypeRepr]) =
      if (types.length > 0)
        types.reduce(or(_, _))
      else
        TypeRepr.of[Nothing]

    def compose(a: ZioType, b: ZioType): ZioType =
      ZioType(and(a.r, b.r), or(a.e, b.e), or(a.a, b.a))

    def or(a: TypeRepr, b: TypeRepr) =
      (a.widen.asType, b.widen.asType) match
        case ('[at], '[bt]) =>
          TypeRepr.of[at | bt].simplified.widen // TODO check that this widens multiple exceptions to `Throwable`

    def and(a: TypeRepr, b: TypeRepr) =
      // if either type is Any, specialize to the thing that is narrower
      val out =
        (a.widen.asType, b.widen.asType) match
          case ('[at], '[bt]) =>
            if (a =:= TypeRepr.of[Any] && b =:= TypeRepr.of[Any])
              TypeRepr.of[Any]
            else if (a =:= TypeRepr.of[Any])
              TypeRepr.of[bt]
            else if (b =:= TypeRepr.of[Any])
              TypeRepr.of[at]
            else
              TypeRepr.of[at with bt]
      out
  }
  protected object ComputeType {
    def fromIR(ir: IR): ZioType = from(ir)
    private def from(ir: IR): ZioType =
      ir match {
        case IR.Pure(code) =>
          ZioType.fromPure(code)

        case IR.FlatMap(monad, valSymbol, body) =>
          from(monad).flatMappedWith(from(body))

        case IR.Map(monad, valSymbol, IR.Pure(term)) =>
          from(monad).mappedWith(term)

        case IR.Monad(code) => ZioType.fromZIO(code)

        case IR.Block(head, tail) =>
          from(tail)

        // Things inside Unsafe blocks that are not inside awaits will be wrapepd into ZIO.attempt
        // which will make their lower-bound Throwable in the MonadifyTries phase.
        // Do not need to do that manually here with the `e` type though.
        case IR.Unsafe(body) =>
          from(body)

        case IR.Match(scrutinee, caseDefs) =>
          // Ultimately the scrutinee will be used if it is pure or lifted, either way we can
          // treat it as a value that will be flatMapped (or Mapped) against the caseDef values.
          val scrutineeType = from(scrutinee)
          // NOTE: We can possibly do the same thing as IR.Try and pass through the Match result tpe, then
          // then use that to figure out what the type should be
          val caseDefTypes = caseDefs.map(caseDef => from(caseDef.rhs))
          val caseDefTotalType = ZioType.composeN(caseDefTypes)
          scrutineeType.flatMappedWith(caseDefTotalType)

        case IR.If(cond, ifTrue, ifFalse) =>
          val condType = from(cond)
          val expressionType = ZioType.compose(from(ifTrue), from(ifFalse))
          condType.flatMappedWith(expressionType)

        case IR.And(left, right) =>
          ZioType.compose(from(left), from(right))

        case IR.Or(left, right) =>
          ZioType.compose(from(left), from(right))

        case IR.Try(tryBlock, caseDefs, outputType, finallyBlock) =>
          val tryBlockType = from(tryBlock)
          // Could possibly try to delve into the case-def types and widen it from there
          // but scala has already computed the total type from the output of the Try term
          // so instead we can just use that.
          // val caseDefType =
          //   ZioType.composeN(
          //     caseDefs.map(caseDef => from(caseDef.rhs))
          //   )
          val caseDefType =
            outputType.asType match
              case '[ZIO[r, e, a]] => ZioType(TypeRepr.of[r].widen, TypeRepr.of[e].widen, TypeRepr.of[a].widen)
              case '[t]            => ZioType(TypeRepr.of[Any].widen, TypeRepr.of[Throwable].widen, TypeRepr.of[t].widen)
          tryBlockType.flatMappedWith(caseDefType)

        case IR.While(cond, body) =>
          val condTpe = from(cond)
          val bodyTpe = from(body)
          val out = condTpe.flatMappedWith(bodyTpe).mappedWithType(TypeRepr.of[Unit])
          println(s"----------- Type: ${condTpe.show}.flatMap(${bodyTpe.show}).map(Unit) => ${out.show}")
          out

        case IR.Parallel(monadics, body) =>
          val monadTypes = monadics.map((monadic, _) => from(monadic))
          val monadsType = ZioType.composeN(monadTypes)
          val bodyType = from(body) // a IR.Leaf will either be a IR.Pure or an IR.Monad, both already have cases here
          monadsType.flatMappedWith(bodyType)
      }
  }
}
