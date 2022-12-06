package zio.direct.core.metaprog

import zio.direct.core.util.WithFormat

trait WithZioType extends MacroBase {
  self: WithFormat =>

  import c.universe._

  protected case class ZioType private (r: Type, e: Type, a: Type) {
    def show = s"ZioType(${Format.Type(r)}, ${Format.Type(e)}, ${Format.Type(a)})"

    def transformR(f: Type => Type) =
      ZioType(f(r), e, a)
    def transformE(f: Type => Type) =
      ZioType(r, f(e), a)
    def transformA(f: Type => Type) =
      ZioType(r, e, f(a))

    def asTypeTuple = (r, e, a)

    def toZioType: Type =
      asTypeTuple match {
        case (r, e, a) =>
          c.typecheck(tq"dev.ZIO[$r, $e, $a]").tpe
      }

    def flatMappedWith(other: ZioType)(implicit typeUnion: TypeUnion) =
      ZioType(ZioType.and(r, other.r), ZioType.or(e, other.e)(typeUnion), other.a)

    def mappedWith(other: Tree) =
      ZioType(r, e, other.tpe)

    def mappedWithType(tpe: Type) =
      ZioType(r, e, tpe)
  }
  protected object ZioType {
    def fromMulti(rs: List[Type], es: List[Type], as: List[Type])(implicit typeUnion: TypeUnion) =
      ZioType(
        ZioType.andN(rs),
        ZioType.orN(es)(typeUnion),
        ZioType.orN(as)
      )

    def fromZIO(zio: Tree) = {
      zio.tpe

      // case '[ZIO[r, e, a]] =>
      //   ZioType(typeOf[r], typeOf[e], typeOf[a])
      // case _ =>
      //   report.errorAndAbort(s"The type of ${Format.Tree(zio)} is not a ZIO. It is: ${Format.Type(zio.tpe)}")
    }

    // In this case the error is considered to be Nothing (since we are not wrapping error handling for pure values)
    // and the environment type is considered to be Any (it will be removed via later `ZioType.union` calls if it can be specialized).
    // Only the output type is used
    def fromPure(Tree: Tree) =
      ZioType(typeOf[Any], typeOf[Nothing], Tree.tpe)

    def apply(r: Type, e: Type, a: Type) =
      new ZioType(r.widen, e.widen, a.widen)

    def composeN(zioTypes: List[ZioType])(implicit typeUnion: TypeUnion): ZioType = {
      val (rs, es, as) = zioTypes.map(zt => (zt.r, zt.e, zt.a)).unzip3
      ZioType(andN(rs), orN(es), andN(as))
    }

    private def andN(types: List[Type]) =
      if (types.length == 1)
        types.head
      else if (types.length > 1)
        types.reduce(and(_, _))
      else
        typeOf[Any]

    private def orN(types: List[Type])(implicit typeUnion: TypeUnion) =
      if (types.length == 1)
        types.head
      else if (types.length > 1)
        types.reduce(or(_, _))
      else
        typeOf[Nothing]

    def compose(a: ZioType, b: ZioType)(implicit typeUnion: TypeUnion): ZioType =
      ZioType(and(a.r, b.r), or(a.e, b.e), or(a.a, b.a))

    def computeCommonBaseClass(a: Type, b: Type): Type = {
      import scala.collection.immutable.ListSet
      if (a =:= typeOf[Nothing] && b =:= typeOf[Nothing]) typeOf[Nothing]
      else if (a =:= typeOf[Nothing]) b
      else if (b =:= typeOf[Nothing]) a
      else if (a =:= typeOf[Any] || b =:= typeOf[Any]) typeOf[Any]
      else {
        val isectRawOpt = ListSet(a.widen.baseClasses: _*).intersect(ListSet(b.widen.baseClasses: _*)).headOption
        val isectRaw =
          isectRawOpt match {
            case Some(value) =>
              a.baseType(value)
            case None =>
              report.warning(s"The types ${a.widen} and ${b.widen} did not have any base-classes in common. Cannot compute a common error type between the two.")
              typeOf[Any]
          }
        isectRaw
      }
    }

    private def or(a: Type, b: Type)(implicit typeUnion: TypeUnion): Type =
      // TODO Back here fix
      ???
      // typeUnion match {
      //   case TypeUnion.OrType =>
      //     (a.widen.asType, b.widen.asType) match
      //       case ('[at], '[bt]) =>
      //         typeOf[at | bt]
      //   case TypeUnion.LeastUpper =>
      //     Embedder.computeCommonBaseClass(a.widen, b.widen)
      // }

    private def and(a: Type, b: Type) = {
      // if either type is Any, specialize to the thing that is narrower
      val out =
        if (a =:= typeOf[Any] && b =:= typeOf[Any])
          typeOf[Any]
        else if (a =:= typeOf[Any])
          b
        else if (b =:= typeOf[Any])
          a
        else {
          // tq"$a with $b"
          // RefinedType(List(a, b), Scopes)
          // do we really need to do c.typecheck? double check that
          // if it's not efficient we can do it once the whole type has been computed
          c.typecheck(tq"$a with $b").tpe
        }
      out
    }
  }
}
