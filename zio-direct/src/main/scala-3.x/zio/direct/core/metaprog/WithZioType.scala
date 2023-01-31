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
import zio.direct.MonadShape

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

  object List3Or6 {
    def unapply[T](list: List[T]) =
      list match {
        case List(a, b, c)          => Some((a, b, c))
        case List(_, _, _, a, b, c) => Some((a, b, c))
        case _                      => None
      }
  }

  // TODO At least check that the A type exists, it has to since it's a value
  class ZioEffectType private (val tpe: TypeRepr, val variances: List[(MonadShape.Letter, MonadShape.Type)]) {
    if (!variances.exists(_._1 == MonadShape.Letter.A))
      report.errorAndAbort("List of MonadShape letters must at least include an A i.e. value-type.")

    private object Index {
      val variancesLetters: List[MonadShape.Letter] = variances.map(_._1)
      val ofR = variancesLetters.indexOf(MonadShape.Letter.R)
      val ofE = variancesLetters.indexOf(MonadShape.Letter.E)
      val ofA = variancesLetters.indexOf(MonadShape.Letter.A)
    }

    private val defaultType = TypeRepr.of[Nothing]

    private object IndexStrict {
      val variancesLettersStrict: List[MonadShape.Letter] = variances.map(_._1)
      val ofR = variancesLettersStrict.indexOf(MonadShape.Letter.R)
      val ofE = variancesLettersStrict.indexOf(MonadShape.Letter.E)
      val ofA = variancesLettersStrict.indexOf(MonadShape.Letter.A)
    }

    private val monadShapeR: MonadShape.Type = variances.find(_._1 == MonadShape.Letter.R).map(_._2).getOrElse(MonadShape.Type.Unused)
    private val monadShapeE: MonadShape.Type = variances.find(_._1 == MonadShape.Letter.E).map(_._2).getOrElse(MonadShape.Type.Unused)
    private val monadShapeA: MonadShape.Type = variances.find(_._1 == MonadShape.Letter.A).map(_._2).getOrElse(MonadShape.Type.Unused)

    private[WithZioType] def composeR(a: TypeRepr, b: TypeRepr)(implicit tu: TypeUnion) = composeLetter(monadShapeR)(a, b)
    private[WithZioType] def composeE(a: TypeRepr, b: TypeRepr)(implicit tu: TypeUnion) = composeLetter(monadShapeE)(a, b)
    private[WithZioType] def composeA(a: TypeRepr, b: TypeRepr)(implicit tu: TypeUnion) = composeLetter(monadShapeA)(a, b)

    private[WithZioType] def composeRs(rs: Chunk[TypeRepr])(implicit tu: TypeUnion) = composeLetterN(monadShapeR)(rs)
    private[WithZioType] def composeEs(es: Chunk[TypeRepr])(implicit tu: TypeUnion) = composeLetterN(monadShapeE)(es)
    private[WithZioType] def composeAs(as: Chunk[TypeRepr])(implicit tu: TypeUnion) = composeLetterN(monadShapeA)(as)

    private def composeLetter(letterShape: MonadShape.Type)(a: TypeRepr, b: TypeRepr)(implicit tu: TypeUnion) = {
      // case match would be better but performance is important here
      import zio.direct.core.metaprog.Embedder.Compose
      if (letterShape == MonadShape.Type.Contravariant) Compose.and(a, b)
      else if (letterShape == MonadShape.Type.Covariant) Compose.or(a, b)
      else defaultType
    }

    private def composeLetterN(letterShape: MonadShape.Type)(letters: Chunk[TypeRepr])(implicit tu: TypeUnion) = {
      // case match would be better but performance is important here
      import zio.direct.core.metaprog.Embedder.Compose
      if (letterShape == MonadShape.Type.Contravariant) Compose.andN(letters)
      else if (letterShape == MonadShape.Type.Covariant) Compose.orN(letters)
      else defaultType
    }

    // Compares erased values of the two effects
    def isEffectOf(other: TypeRepr): Boolean =
      other match {
        case AppliedType(root, args) if (args.length == variances.length) =>
          root =:= this.tpe
        case Dealiased(AppliedType(root, args)) if (args.length == variances.length) =>
          root =:= this.tpe
        case _ =>
          false
      }

    // array size of arrays if they need to be reconstructed with elements inside
    private object ReconstructHelpers {
      val lettersArraySize = {
        def oneIfExists(index: Int) = if (index != -1) 1 else 0
        oneIfExists(IndexStrict.ofR) + oneIfExists(IndexStrict.ofE) + oneIfExists(IndexStrict.ofA)
      }
      def ifExistFillElement(index: Int, element: TypeRepr, arr: Array[Any]) =
        if (index != -1) arr(index) = element
    }

    def reconstruct(r: TypeRepr, e: TypeRepr, a: TypeRepr) =
      val arr = new Array[Any](ReconstructHelpers.lettersArraySize)
      ReconstructHelpers.ifExistFillElement(IndexStrict.ofR, r, arr)
      ReconstructHelpers.ifExistFillElement(IndexStrict.ofE, e, arr)
      ReconstructHelpers.ifExistFillElement(IndexStrict.ofA, a, arr)
      AppliedType(tpe, arr.asInstanceOf[Array[TypeRepr]].toList)

    private val runCallMatcher = new RunCall(this)

    object RunCall {
      def unapply(term: Term) = runCallMatcher.unapply(term)
    }

    object ListUnapply {
      def unapply(tpes: List[TypeRepr]) =
        if (tpes.length != variances.length) None
        // Again, this could be achieved via zipping but I want it to be more performant since this check
        // will be run for every type of every parameter in the scala tree.
        else {
          val rTpe = if (Index.ofR != -1) tpes(Index.ofR) else defaultType
          val eTpe = if (Index.ofR != -1) tpes(Index.ofE) else defaultType
          val aTpe = if (Index.ofR != -1) tpes(Index.ofA) else defaultType
          Some((rTpe, eTpe, aTpe))
        }
    }

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
        // In the type-spec, need to have a way to specify how many args
        case AppliedType(root, ListUnapply(a, b, c)) if (root =:= this.tpe) =>
          Some((a, b, c))
        case Dealiased(AppliedType(root, ListUnapply(a, b, c))) if (root =:= this.tpe) =>
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
          case AppliedType(root, args) => // List(a, b, c) (TODO should have some kind of way to specify # params used?)
            root
          case _ =>
            report.errorAndAbort(s"Could not identify the effect type of: ${tpe.show}")
      new ZioEffectType(
        rootType,
        List(
          (MonadShape.Letter.R, MonadShape.Type.Contravariant),
          (MonadShape.Letter.E, MonadShape.Type.Covariant),
          (MonadShape.Letter.A, MonadShape.Type.Covariant)
        )
      )
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
      ZioType(this.effectType)(effectType.composeR(r, other.r), effectType.composeE(e, other.e)(typeUnion), other.a)

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
        effectType.composeRs(rs),
        effectType.composeEs(es)(typeUnion),
        effectType.composeAs(as)
      )

    def composeN(zioTypes: NonEmptyChunk[ZioType])(implicit typeUnion: TypeUnion): ZioType =
      val et = validateSameEffect(zioTypes, "N-composition")
      val (rs, es, as) = zioTypes.map(zt => (zt.r, zt.e, zt.a)).unzip3
      ZioType(validateSameEffect(zioTypes, "N-composition"))(et.composeRs(rs), et.composeEs(es), et.composeAs(as))

    private def validateSameEffect(types: NonEmptyChunk[ZioType], label: String): ZioEffectType = {
      types.map(_.effectType).reduce((a, b) => {
        if (a != b)
          report.errorAndAbort(s"Different effect types encountered: ${a.tpe.show} and ${b.tpe.show}")
        else
          a
      })
    }

    def compose(a: ZioType, b: ZioType)(implicit typeUnion: TypeUnion): ZioType =
      val et = validateSameEffect(NonEmptyChunk(a, b), "a/b composition")
      ZioType(et)(et.composeR(a.r, b.r), et.composeE(a.e, b.e), et.composeA(a.a, b.a))
  }

  private class RunCall(effectType: ZioEffectType) {
    import Extractors._
    def unapply(tree: Tree): Option[Expr[_]] = {
      tree match
        case DottyExtensionCall(invocation @ DirectRunCallAnnotated.Term(), effect) =>
          if (effectType.isEffectOf(effect.tpe))
            Some(effect.asExpr)
          else
            None

        case _ => None
    }
  }
}
