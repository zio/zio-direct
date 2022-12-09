package zio.direct.core.metaprog

import zio.direct.core.util.WithFormat
import scala.annotation.nowarn

trait WithZioType extends MacroBase {
  self: WithFormat =>

  import c.universe._

  protected case class ZioType private (val r: Type, val e: Type, val a: Type) {
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
          c.typecheck(tq"zio.ZIO[$r, $e, $a]", c.TYPEmode).tpe
      }

    def flatMappedWith(other: ZioType)(implicit typeUnion: TypeUnion) =
      ZioType(ZioType.and(r, other.r), ZioType.or(e, other.e)(typeUnion), other.a)

    def mappedWith(other: Tree) =
      ZioType(r, e, c.typecheck(other).tpe)

    def mappedWithType(tpe: Type) =
      ZioType(r, e, tpe)
  }
  protected object ZioType {
    def fromPrimaryWithOthers(primary: ZioType)(others: ZioType*)(implicit typeUnion: TypeUnion) = {
      fromMultiTypes(
        primary.r +: others.map(_.r).toList,
        primary.e +: others.map(_.e).toList,
        List(primary.a)
      )(typeUnion)
    }

    def fromUnitWithOthers(others: ZioType*)(implicit typeUnion: TypeUnion) = {
      fromMultiTypes(
        others.map(_.r).toList,
        others.map(_.e).toList,
        List(typeOf[Unit])
      )(typeUnion)
    }

    def fromMultiTypes(rs: List[Type], es: List[Type], as: List[Type])(implicit typeUnion: TypeUnion) =
      ZioType(
        ZioType.andN(rs),
        ZioType.orN(es)(typeUnion),
        ZioType.orN(as)
      )

    // Isolate all inputs to ZioType to fromPure and fromZIO that way we can
    // be sure that everything coming into ZioType is typechecked and we don't have
    // any surprises with `.tpe`s being null.
    def fromZIO(treeRaw: Tree) = {
      val tree = c.typecheck(treeRaw)
      val (r, e, a) = decomposeZioTypeFromTree(tree)
      ZioType(r, e, a)
    }

    private def decomposeZioTypeFromTree(zioTree: Tree) =
      zioTree.tpe.dealias.widen match {
        case TypeRef(_, cls, List(r, e, a)) if (cls.isClass && cls.asClass.fullName == "zio.ZIO") =>
          (r, e, a)
        // case tq"ZIO[$r, $e, $a]" =>
        //   (r.tpe, e.tpe, a.tpe)
        case _ =>
          // TODO show raw on a ghigh level of verbosity
          report.errorAndAbort(s"The type of ${Format.Tree(zioTree)} is not a ZIO. It is: ${Format.Type(zioTree.tpe)}")
      }

    // Isolate all inputs to ZioType to fromPure and fromZIO that way we can
    // be sure that everything coming into ZioType is typechecked and we don't have
    // any surprises with `.tpe`s being null.
    def fromPure(treeRaw: Tree) = {
      val tree = c.typecheck(treeRaw)
      // In some cases the type of the tree was not computed so it will come out as null. In that case we are forced to do a typecheck
      val tpe =
        if (tree.tpe != null) tree.tpe
        else c.typecheck(tree).tpe
      ZioType(typeOf[Any], typeOf[Nothing], tpe)
    }

    def apply(r: Type, e: Type, a: Type) = {
      // Scala2 has a tendancy to think .tpe of something is null, throw more specific exceptions if that is encountered
      if (r == null) throw new NullPointerException("R parameter is null")
      if (e == null) throw new NullPointerException("E parameter is null")
      if (a == null) throw new NullPointerException("A parameter is null")
      new ZioType(r.widen, e.widen, a.widen)
    }

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
              val out = a.baseType(value)
              // println(s"======== Reduce ${show(a)} and ${show(b)} - to - ${show(out)}")
              out
            case None =>
              report.warning(s"The types ${a.widen} and ${b.widen} did not have any base-classes in common. Cannot compute a common error type between the two.")
              typeOf[Any]
          }
        isectRaw
      }
    }

    // Not using the typeUnion here because Scala 2 has no type unions
    @nowarn
    private def or(a: Type, b: Type)(implicit typeUnion: TypeUnion): Type = {
      val out = computeCommonBaseClass(a.widen, b.widen)
      // println(s"============= Common BaseClass of ${show(a)} and ${show(b)} -is- ${show(out)}")
      out
    }

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
          c.typecheck(tq"$a with $b", c.TYPEmode).tpe
        }
      // println(s"============= Reduced ${show(a)} and ${show(b)} -to- ${show(out)}")
      out
    }
  }
}
