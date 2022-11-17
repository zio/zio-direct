package zio.direct.core.norm

import zio.direct.core.metaprog.WithIR
import scala.quoted._
import zio.direct.core.util.Format
import zio.ZIO
import zio.direct.core.metaprog.WithPrintIR
import zio.direct.core.util.WithInterpolator
import zio.direct.core.util.TraceType
import zio.direct.core.metaprog.Instructions

trait WithComputeType {
  self: WithIR with WithInterpolator with WithPrintIR =>

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  protected case class ZioType(r: TypeRepr, e: TypeRepr, a: TypeRepr) {
    def show = s"ZioType(${Format.TypeRepr(r)}, ${Format.TypeRepr(e)}, ${Format.TypeRepr(a)})"

    def transformR(f: TypeRepr => TypeRepr) =
      ZioType(f(r), e, a)
    def transformE(f: TypeRepr => TypeRepr) =
      ZioType(r, f(e), a)
    def transformA(f: TypeRepr => TypeRepr) =
      ZioType(r, e, f(a))

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
      // if (a =:= TypeRepr.of[Nothing] && b =:= TypeRepr.of[Nothing]) TypeRepr.of[Nothing]
      // else
      //   val base = a.baseType(b.typeSymbol)
      //   // println(s"---------- Base of: ${Format.TypeRepr(a)} and ${Format.TypeRepr(b)} -> ${Format.TypeRepr(TypeRepr.of[at | bt].simplified.widen)}")
      //   base
      (a.widen.asType, b.widen.asType) match
        case ('[at], '[bt]) =>
          // val base = a.widen.baseType(b.widen.typeSymbol)
          // val msg =
          //   s"---------- Or(${Format.TypeRepr(a)}, ${Format.TypeRepr(b)}) -> ${Format.TypeRepr(TypeRepr.of[at | bt].simplified.widen)} / ${base}\n"
          //     + { val pos = Symbol.spliceOwner.pos.get; s"at: ${pos.sourceFile.path}:${pos.startLine + 1}" }
          // if (msg.contains("Or(Int, String)")) println(msg)
          // if (msg.contains("Or(Int, String)")) {
          //   println(msg)
          //   println(
          //     s"""|===== Base is NoType
          //         |${base.toString == "NoType"}
          //         |""".stripMargin
          //   )
          //   println((new RuntimeException).getStackTrace().take(20).mkString("\n"))
          //   // base =:= TypeRepr.of[Nothing]
          // }
          TypeRepr.of[at | bt].simplified.widen

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
  object ComputeType {
    def fromIR(ir: IR)(implicit instructions: Instructions): ZioType =
      new ComputeType(instructions).apply(ir)
  }

  class ComputeType private (instructions: Instructions) {
    val interp = Interpolator(TraceType.TypeCompute)(instructions)
    import interp._

    def apply(ir: IR): ZioType =
      ir match {
        case IR.Pure(code) =>
          ZioType.fromPure(code)

        case IR.FlatMap(monad, valSymbol, body) =>
          apply(monad).flatMappedWith(apply(body))

        case IR.Map(monad, valSymbol, IR.Pure(term)) =>
          apply(monad).mappedWith(term)

        case IR.Monad(code) => ZioType.fromZIO(code)

        case IR.Block(head, tail) =>
          apply(tail)

        case IR.Fail(error) =>
          // TODO need to check that environment is correct in this case
          val bodyError = apply(error)
          println(s"--------- Rendering error type: ${bodyError.show}")
          ZioType(bodyError.r, bodyError.a.widenTermRefByName, TypeRepr.of[Nothing])

        // Things inside Unsafe blocks that are not inside awaits will be wrapepd into ZIO.attempt
        // which will make their lower-bound Throwable in the MonadifyTries phase.
        // Do not need to do that manually here with the `e` type though.
        case IR.Unsafe(body) =>
          apply(body)

        case IR.Match(scrutinee, caseDefs) =>
          // Ultimately the scrutinee will be used if it is pure or lifted, either way we can
          // treat it as a value that will be flatMapped (or Mapped) against the caseDef values.
          val scrutineeType = apply(scrutinee)
          // NOTE: We can possibly do the same thing as IR.Try and pass through the Match result tpe, then
          // then use that to figure out what the type should be
          val caseDefTypes = caseDefs.map(caseDef => apply(caseDef.rhs))
          val caseDefTotalType = ZioType.composeN(caseDefTypes)
          scrutineeType.flatMappedWith(caseDefTotalType)

        case IR.If(cond, ifTrue, ifFalse) =>
          val condType = apply(cond)
          val expressionType = ZioType.compose(apply(ifTrue), apply(ifFalse))
          condType.flatMappedWith(expressionType)

        case IR.And(left, right) =>
          ZioType.compose(apply(left), apply(right))

        case IR.Or(left, right) =>
          ZioType.compose(apply(left), apply(right))

        case IR.Try(tryBlock, caseDefs, outputType, finallyBlock) =>
          val tryBlockType = apply(tryBlock)
          // We get better results by doing into the case-defs and computing the union-type of them
          // than we do by getting the information from outputType because outputType is limited
          // by Scala's ability to understand the try-catch. For example, if we infer from outputType,
          // the error-type will never be more concrete that `Throwable`.
          val caseDefType =
            ZioType.composeN(caseDefs.map(caseDef => apply(caseDef.rhs)))
          // Could also compute from outputType but this has worse results, see comment above.
          // val caseDefType =
          //   outputType.asType match
          //     case '[ZIO[r, e, a]] => ZioType(TypeRepr.of[r].widen, TypeRepr.of[e].widen, TypeRepr.of[a].widen)
          //     case '[t]            => ZioType(TypeRepr.of[Any].widen, TypeRepr.of[Throwable].widen, TypeRepr.of[t].widen)
          tryBlockType.flatMappedWith(caseDefType)

        case IR.While(cond, body) =>
          val condTpe = apply(cond)
          val bodyTpe = apply(body)
          condTpe.flatMappedWith(bodyTpe).mappedWithType(TypeRepr.of[Unit])

        case IR.Parallel(monadics, body) =>
          val monadTypes = monadics.map((monadic, _) => apply(monadic))
          val bodyType = apply(body) // a IR.Leaf will either be a IR.Pure or an IR.Monad, both already have cases here
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
            ZioType.orN(bodyType.e +: monadTypes.map(_.e)),
            bodyType.a
          )
      }
  }
}
