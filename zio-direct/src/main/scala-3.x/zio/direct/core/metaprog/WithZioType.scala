package zio.direct.core.metaprog

import scala.quoted._
import pprint._
import fansi.Str
import zio.direct.core.util.Format
import zio.ZIO

trait WithZioType {
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

    def flatMappedWith(other: ZioType)(implicit typeUnion: TypeUnion) =
      ZioType(ZioType.and(r, other.r), ZioType.or(e, other.e)(typeUnion), other.a)

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

    def composeN(zioTypes: List[ZioType])(implicit typeUnion: TypeUnion): ZioType =
      val (rs, es, as) = zioTypes.map(zt => (zt.r, zt.e, zt.a)).unzip3
      ZioType(andN(rs), orN(es), andN(as))

    def andN(types: List[TypeRepr]) =
      if (types.length > 0)
        types.reduce(and(_, _))
      else
        TypeRepr.of[Any]

    def orN(types: List[TypeRepr])(implicit typeUnion: TypeUnion) =
      if (types.length > 0)
        types.reduce(or(_, _))
      else
        TypeRepr.of[Nothing]

    def compose(a: ZioType, b: ZioType)(implicit typeUnion: TypeUnion): ZioType =
      ZioType(and(a.r, b.r), or(a.e, b.e), or(a.a, b.a))

    def or(a: TypeRepr, b: TypeRepr)(implicit typeUnion: TypeUnion) =
      typeUnion match {
        case TypeUnion.OrType =>
          (a.widen.asType, b.widen.asType) match
            case ('[at], '[bt]) =>
              TypeRepr.of[at | bt]
        case TypeUnion.LeastUpper =>
          Embedder.computeCommonBaseClass(a.widen, b.widen)
      }

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
}
