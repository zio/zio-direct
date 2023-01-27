package zio.direct.core.metaprog

import scala.quoted._
import pprint._
import fansi.Str
import zio.direct.core.util.Format
import zio.direct.Internal.Marker
import zio.direct.core.metaprog.Extractors.Dealiased
import zio.NonEmptyChunk
import zio.direct.core.util.ThrowableOps
import zio.Chunk

trait WithZioType {
  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  class ZioValue(val term: Term, val zpe: ZioType) {
    def expr: Expr[_] = term.asExpr
  }

  implicit class TermOps(term: Term) {
    def toZioValue(zpe: ZioType) = ZioValue(term, zpe)
  }
  implicit class ExprOps(expr: Expr[_]) {
    def toZioValue(zpe: ZioType) = ZioValue(expr, zpe)
  }

  // TODO when we support non-zio values, will need to have one of these for each supported type
  object ZioValue {
    def apply(term: Term, zpe: ZioType) = new ZioValue(term, zpe)
    def apply(expr: Expr[_], zpe: ZioType) = new ZioValue(expr.asTerm, zpe)
  }

  case class ZioEffectTypeContext(zet: ZioEffectType)

  class ZioEffectType private (val tpe: TypeRepr) {
    // Compares erased values of the two effects
    def isEffectOf(other: TypeRepr): Boolean =
      other match {
        case AppliedType(root, List(_, _, _)) =>
          root == this.tpe
        case Dealiased(AppliedType(root, List(_, _, _))) =>
          root == this.tpe
        case _ =>
          false
      }
    def reconstruct(r: TypeRepr, e: TypeRepr, a: TypeRepr) =
      AppliedType(tpe, List(r, e, a))

    /**
     * Similar to something like
     * {{
     * zioStatement.tpe.asType match
     *  case '[ZIO[a, b, c]] => (TypeRepr.of[a], TypeRepr.of[b], TypeRepr.of[c])
     * }}
     * However, it works with whatever the internal type is.
     */
    def unapply(otherTpe: TypeRepr) =
      otherTpe match
        case AppliedType(root, List(a, b, c)) if (root =:= this.tpe) =>
          Some((a, b, c))
        case Dealiased(AppliedType(root, List(a, b, c))) if (root =:= this.tpe) =>
          Some((a, b, c))
        case _ =>
          None
  }
  object ZioEffectType {
    def of[F[_, _, _]: Type]: ZioEffectType = {
      val stmt = '{ ???.asInstanceOf[F[Marker.A, Marker.B, Marker.C]] }
      val tpe = stmt.asTerm.tpe
      val rootType =
        tpe match
          case AppliedType(root, List(a, b, c)) =>
            root
          case _ =>
            report.errorAndAbort(s"Could not identify the effect type of: ${tpe}")
      new ZioEffectType(rootType)
    }
  }

  // TODO Add the zio effect context to the ZIO type or in the toZioType exressor?
  protected case class ZioType private (val effectType: ZioEffectType)(val r: TypeRepr, val e: TypeRepr, val a: TypeRepr) {
    def show = s"ZioType(${Format.TypeRepr(r)}, ${Format.TypeRepr(e)}, ${Format.TypeRepr(a)})"

    def transformR(f: TypeRepr => TypeRepr) =
      ZioType(this.effectType)(f(r), e, a)
    def transformE(f: TypeRepr => TypeRepr) =
      ZioType(this.effectType)(r, f(e), a)
    def transformA(f: TypeRepr => TypeRepr) =
      ZioType(this.effectType)(r, e, f(a))

    def value = a
    def monad = toZioType
    def valueType = value.asType
    def monadType = monad.asType
    def monadAndValueTypes = (valueType, monadType)

    def asTypeTuple = (r.asType, e.asType, a.asType)

    def toZioType: TypeRepr =
      effectType.reconstruct(r, e, a)

    // TODO Validate that `other` has same effectType
    def flatMappedWith(other: ZioType)(implicit typeUnion: TypeUnion) =
      ZioType(this.effectType)(ZioType.and(r, other.r), ZioType.or(e, other.e)(typeUnion), other.a)

    def mappedWith(other: Term) =
      ZioType(this.effectType)(r, e, other.tpe)

    def mappedWithType(tpe: TypeRepr) =
      ZioType(this.effectType)(r, e, tpe)
  }

  /** ------------------------------------- ZioType Constructors ----------------------------------- */
  protected object ZioType {

    def Unit(effectType: ZioEffectType) = fromPure(effectType)('{ () }.asTerm)

    def fromMonad(effectType: ZioEffectType)(zio: Term) =
      zio.tpe match
        case effectType(r, e, a) =>
          ZioType(effectType)(r, e, a)
        case _ =>
          report.errorAndAbort(s"The type of ${Format.Term(zio)} is not a ZIO. It is: ${Format.TypeRepr(zio.tpe)}")

    // In this case the error is considered to be Nothing (since we are not wrapping error handling for pure values)
    // and the environment type is considered to be Any (it will be removed via later `ZioType.union` calls if it can be specialized).
    // Only the output type is used
    def fromPure(effectType: ZioEffectType)(term: Term) =
      ZioType(effectType)(TypeRepr.of[Any], TypeRepr.of[Nothing], term.tpe)

    def apply(effectType: ZioEffectType)(r: TypeRepr, e: TypeRepr, a: TypeRepr) =
      new ZioType(effectType)(r.widenTermRefByName, e.widenTermRefByName, a.widenTermRefByName)

    def fromPrimaryWithOthers(primary: ZioType)(others: ZioType*)(implicit typeUnion: TypeUnion) = {
      fromMultiTypes(validateSameEffect(NonEmptyChunk(primary, others: _*), "N+primary composition"))(
        NonEmptyChunk(primary.r, others.map(_.r): _*),
        NonEmptyChunk(primary.e, others.map(_.e): _*),
        NonEmptyChunk(primary.a)
      )(typeUnion)
    }

    def fromUnitWithOthers(others: NonEmptyChunk[ZioType])(implicit typeUnion: TypeUnion) = {
      fromMultiTypes(validateSameEffect(others, "N+unit composition"))(
        others.map(_.r),
        others.map(_.e),
        NonEmptyChunk(TypeRepr.of[Unit])
      )(typeUnion)
    }

    private def fromMultiTypes(effectType: ZioEffectType)(rs: NonEmptyChunk[TypeRepr], es: NonEmptyChunk[TypeRepr], as: NonEmptyChunk[TypeRepr])(implicit typeUnion: TypeUnion) =
      ZioType(effectType)(
        ZioType.andN(rs),
        ZioType.orN(es)(typeUnion),
        ZioType.orN(as)
      )

    def composeN(zioTypes: NonEmptyChunk[ZioType])(implicit typeUnion: TypeUnion): ZioType =
      val (rs, es, as) = zioTypes.map(zt => (zt.r, zt.e, zt.a)).unzip3
      ZioType(validateSameEffect(zioTypes, "N-composition"))(andN(rs), orN(es), andN(as))

    private def validateSameEffect(types: NonEmptyChunk[ZioType], label: String): ZioEffectType = {
      types.map(_.effectType).reduce((a, b) => {
        if (a != b)
          report.errorAndAbort(s"Different effect types encountered: ${a.tpe.show} and ${b.tpe.show}")
        else
          a
      })
    }

    private def andN(types: Chunk[TypeRepr]) =
      if (types.length == 1)
        types.head
      else if (types.length > 1)
        types.reduce(and(_, _))
      else
        TypeRepr.of[Any]

    private def orN(types: Chunk[TypeRepr])(implicit typeUnion: TypeUnion) =
      if (types.length == 1)
        types.head
      else if (types.length > 1)
        types.reduce(or(_, _))
      else
        TypeRepr.of[Nothing]

    def compose(a: ZioType, b: ZioType)(implicit typeUnion: TypeUnion): ZioType =
      ZioType(validateSameEffect(NonEmptyChunk(a, b), "a/b composition"))(and(a.r, b.r), or(a.e, b.e), or(a.a, b.a))

    private def or(a: TypeRepr, b: TypeRepr)(implicit typeUnion: TypeUnion) =
      typeUnion match {
        case TypeUnion.OrType =>
          (a.widen.asType, b.widen.asType) match
            case ('[at], '[bt]) =>
              TypeRepr.of[at | bt]
        case TypeUnion.LeastUpper =>
          Embedder.computeCommonBaseClass(a.widen, b.widen)
      }

    private def and(a: TypeRepr, b: TypeRepr) =
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
}
