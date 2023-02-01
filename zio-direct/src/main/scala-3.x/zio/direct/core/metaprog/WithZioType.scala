package zio.direct.core.metaprog

import scala.quoted._
import pprint._
import fansi.Str
import zio.direct.core.util.Format
import zio.direct.Internal.Marker
import zio.direct.core.metaprog.Extractors.Dealiased
import zio.direct.core.util.ThrowableOps
import zio.direct.MonadShape
import zio.direct.MonadModel
import zio.direct.core.metaprog.Embedder.Compose

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

  // case class ZioEffectTypeContext(zet: ZioEffectType)
  // object List3Or6 {
  //   def unapply[T](list: List[T]) =
  //     list match {
  //       case List(a, b, c)          => Some((a, b, c))
  //       case List(_, _, _, a, b, c) => Some((a, b, c))
  //       case _                      => None
  //     }
  // }

  // TODO At least check that the A type exists, it has to since it's a value
  class ZioEffectType private (val tpe: TypeRepr, val variances: Array[(MonadShape.Letter, MonadShape.Variance)]) {
    if (!variances.exists(_._1 == MonadShape.Letter.A))
      report.errorAndAbort("List of MonadShape letters must at least include an A i.e. value-type.")

    private val variancesLetters: Array[MonadShape.Letter] = variances.map(_._1)
    private val indexOfR = variancesLetters.indexOf(MonadShape.Letter.R)
    private val indexOfE = variancesLetters.indexOf(MonadShape.Letter.E)
    private val indexOfA = variancesLetters.indexOf(MonadShape.Letter.A)

    private val defaultType = TypeRepr.of[Nothing]

    private val variancesLettersStrict: Array[MonadShape.Letter] = variances.filter(_._1 != MonadShape.Letter.Other).map(_._1)
    private val indexStrictOfR = variancesLettersStrict.indexOf(MonadShape.Letter.R)
    private val indexStrictOfE = variancesLettersStrict.indexOf(MonadShape.Letter.E)
    private val indexStrictOfA = variancesLettersStrict.indexOf(MonadShape.Letter.A)

    private val monadShapeR: MonadShape.Variance = variances.find(_._1 == MonadShape.Letter.R).map(_._2).getOrElse(MonadShape.Variance.Unused)
    private val monadShapeE: MonadShape.Variance = variances.find(_._1 == MonadShape.Letter.E).map(_._2).getOrElse(MonadShape.Variance.Unused)
    private val monadShapeA: MonadShape.Variance = variances.find(_._1 == MonadShape.Letter.A).map(_._2).getOrElse(MonadShape.Variance.Unused)

    def composeR(a: TypeRepr, b: TypeRepr)(implicit tu: TypeUnion) = composeLetter(monadShapeR)(a, b)
    def composeE(a: TypeRepr, b: TypeRepr)(implicit tu: TypeUnion) = composeLetter(monadShapeE)(a, b)
    def composeA(a: TypeRepr, b: TypeRepr)(implicit tu: TypeUnion) = composeLetter(monadShapeA)(a, b)

    def composeRs(rs: List[TypeRepr])(implicit tu: TypeUnion) = composeLetterN(monadShapeR)(rs)
    def composeEs(es: List[TypeRepr])(implicit tu: TypeUnion) = composeLetterN(monadShapeE)(es)
    def composeAs(as: List[TypeRepr])(implicit tu: TypeUnion) = composeLetterN(monadShapeA)(as)

    private def composeLetter(letterShape: MonadShape.Variance)(a: TypeRepr, b: TypeRepr)(implicit tu: TypeUnion) = {
      // case match would be better but performance is important here
      import zio.direct.core.metaprog.Embedder.Compose
      if (letterShape == MonadShape.Variance.Contravariant) Compose.and(a, b)
      else if (letterShape == MonadShape.Variance.Covariant) Compose.or(a, b)
      else defaultType
    }

    private def composeLetterN(letterShape: MonadShape.Variance)(letters: List[TypeRepr])(implicit tu: TypeUnion) = {
      // case match would be better but performance is important here
      import zio.direct.core.metaprog.Embedder.Compose
      if (letterShape == MonadShape.Variance.Contravariant) Compose.andN(letters)
      else if (letterShape == MonadShape.Variance.Covariant) Compose.orN(letters)
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
    private def oneIfExists(index: Int) = if (index != -1) 1 else 0
    val lettersArraySize =
      oneIfExists(indexStrictOfR) + oneIfExists(indexStrictOfE) + oneIfExists(indexStrictOfA)
    private def ifExistFillElement(index: Int, element: TypeRepr, arr: Array[Any]) =
      if (index != -1) arr(index) = element

    def reconstruct(r: TypeRepr, e: TypeRepr, a: TypeRepr) =
      val arr = new Array[Any](lettersArraySize)
      ifExistFillElement(indexStrictOfR, r, arr)
      ifExistFillElement(indexStrictOfE, e, arr)
      ifExistFillElement(indexStrictOfA, a, arr)
      println(s"---------------- Fill Arr (${lettersArraySize}): ${arr.asInstanceOf[Array[TypeRepr]].toList}")
      AppliedType(tpe, arr.asInstanceOf[Array[TypeRepr]].toList)

    // assumes lengths of variances and types list are the same, things that calls this needs to check that
    private def extractTypes(tpes: List[TypeRepr]) =
      // Again, this could be achieved via zipping but I want it to be more performant since this check
      // will be run for every type of every parameter in the scala tree.
      val rTpe = if (indexOfR != -1) tpes(indexOfR) else defaultType
      val eTpe = if (indexOfE != -1) tpes(indexOfE) else defaultType
      val aTpe = if (indexOfA != -1) tpes(indexOfA) else defaultType
      (rTpe, eTpe, aTpe)

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
        case AppliedType(root, typeArgs) if (typeArgs.length == variances.length && root =:= this.tpe) =>
          Some(extractTypes(typeArgs))
        case Dealiased(AppliedType(root, typeArgs)) if (typeArgs.length == variances.length && root =:= this.tpe) =>
          Some(extractTypes(typeArgs))
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

      val monadModel = computeMonadModel[F](tpe)
      println(s"============== Letters and variances: ${monadModel}")

      new ZioEffectType(
        rootType,
        monadModel.toArray
        // Array(
        //   (MonadShape.Letter.R, MonadShape.Variance.Contravariant),
        //   (MonadShape.Letter.E, MonadShape.Variance.Covariant),
        //   (MonadShape.Letter.A, MonadShape.Variance.Covariant)
        // )
      )
    }

    private def computeMonadModel[F[_, _, _]: Type](tpe: TypeRepr) = {
      val monadModel = Expr.summon[MonadModel[F]].getOrElse {
        report.errorAndAbort(s"Could not summon a MonadModel for: ${tpe.show}")
      }
      val monadModelVariancesMemberSymbol = monadModel.asTerm.tpe.typeSymbol.typeMembers.find(_.name == "Variances").getOrElse {
        report.errorAndAbort(s"""Did not found a `Variances` property on the type `${tpe.show}`""")
        // term: ${monadModel.asTerm.tpe.termSymbol.typeMembers}
        // type: ${monadModel.asTerm.tpe.typeSymbol.typeMembers}
      }
      val monadModelLettersMemberSymbol = monadModel.asTerm.tpe.typeSymbol.typeMembers.find(_.name == "Letters").getOrElse {
        report.errorAndAbort(s"Did not found a `Letters` property on the type `${tpe.show}`")
      }

      val monadModelVariancesList = monadModel.asTerm.select(monadModelVariancesMemberSymbol).tpe.widen
      val monadModelLettersList = monadModel.asTerm.select(monadModelLettersMemberSymbol).tpe.widen

      // // println(s"------------- Types: ${monadModelVariancesList}")

      val monadShapeVariances =
        monadModelVariancesList match
          case TypeBounds(AppliedType(variances /* todo check name of this? */, args), _) => args

      //     // case '[MonadShape.Variances1[a]]                   => List(TypeRepr.of[a])
      //     // case '[MonadShape.Variances2[a, b]]                => List(TypeRepr.of[a], TypeRepr.of[b])
      //     // case '[MonadShape.Variances3[a, b, c]]             => List(TypeRepr.of[a], TypeRepr.of[b], TypeRepr.of[c])
      //     // case '[MonadShape.Variances4[a, b, c, d]]          => List(TypeRepr.of[a], TypeRepr.of[b], TypeRepr.of[c], TypeRepr.of[d])
      //     // case '[MonadShape.Variances5[a, b, c, d, e]]       => List(TypeRepr.of[a], TypeRepr.of[b], TypeRepr.of[c], TypeRepr.of[d], TypeRepr.of[e])
      //     // case '[MonadShape.Variances6[a, b, c, d, e, f]]    => List(TypeRepr.of[a], TypeRepr.of[b], TypeRepr.of[c], TypeRepr.of[d], TypeRepr.of[e], TypeRepr.of[f])
      //     // case '[MonadShape.Variances7[a, b, c, d, e, f, g]] => List(TypeRepr.of[a], TypeRepr.of[b], TypeRepr.of[c], TypeRepr.of[d], TypeRepr.of[e], TypeRepr.of[f], TypeRepr.of[g])

      val monadShapeLetters =
        monadModelLettersList match
          case TypeBounds(AppliedType(letters /* todo check name of this? */, args), _) => args

      //     // case '[MonadShape.Letters1[a]]                   => List(TypeRepr.of[a])
      //     // case '[MonadShape.Letters2[a, b]]                => List(TypeRepr.of[a], TypeRepr.of[b])
      //     // case '[MonadShape.Letters3[a, b, c]]             => List(TypeRepr.of[a], TypeRepr.of[b], TypeRepr.of[c])
      //     // case '[MonadShape.Letters4[a, b, c, d]]          => List(TypeRepr.of[a], TypeRepr.of[b], TypeRepr.of[c], TypeRepr.of[d])
      //     // case '[MonadShape.Letters5[a, b, c, d, e]]       => List(TypeRepr.of[a], TypeRepr.of[b], TypeRepr.of[c], TypeRepr.of[d], TypeRepr.of[e])
      //     // case '[MonadShape.Letters6[a, b, c, d, e, f]]    => List(TypeRepr.of[a], TypeRepr.of[b], TypeRepr.of[c], TypeRepr.of[d], TypeRepr.of[e], TypeRepr.of[f])
      //     // case '[MonadShape.Letters7[a, b, c, d, e, f, g]] => List(TypeRepr.of[a], TypeRepr.of[b], TypeRepr.of[c], TypeRepr.of[d], TypeRepr.of[e], TypeRepr.of[f], TypeRepr.of[g])

      // // println(s"============ MonadModelTypes: ${monadShapeTypes.map(_.show)}")
      // // println(s"============ MonadModelLetters: ${monadShapeLetters.map(_.show)}")

      if (monadShapeVariances.length != monadShapeLetters.length)
        report.errorAndAbort(s"The type `${tpe.show}` does not have an equally sized list of Letters and Variances but needs to. (${monadShapeLetters.map(_.show)} vs ${monadShapeVariances.map(_.show)})")

      def letterTypeToLetter(letter: TypeRepr) =
        if (letter =:= TypeRepr.of[MonadShape.Letter.R]) MonadShape.Letter.R
        else if (letter =:= TypeRepr.of[MonadShape.Letter.E]) MonadShape.Letter.E
        else if (letter =:= TypeRepr.of[MonadShape.Letter.A]) MonadShape.Letter.A
        else MonadShape.Letter.Other

      def varianceTypeToVariance(variance: TypeRepr) =
        if (variance =:= TypeRepr.of[MonadShape.Variance.Contravariant]) MonadShape.Variance.Contravariant
        else if (variance =:= TypeRepr.of[MonadShape.Variance.Covariant]) MonadShape.Variance.Covariant
        else MonadShape.Variance.Unused

      val letters = monadShapeLetters.map(letterTypeToLetter(_))
      val variances = monadShapeVariances.map(varianceTypeToVariance(_))
      letters.zip(variances)
    }
  }

  // TODO Add the zio effect context to the ZIO type or `in the toZioType exressor?
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

    def fromPrimaryWithOthers(primary: ZioType)(others: List[ZioType])(implicit typeUnion: TypeUnion) = {
      fromMultiTypes(validateSameEffect(primary +: others, "N+primary composition"))(
        primary.r +: others.map(_.r),
        primary.e +: others.map(_.e),
        List(primary.a)
      )(typeUnion)
    }

    def fromUnitWithOthers(others: List[ZioType])(implicit typeUnion: TypeUnion) = {
      fromMultiTypes(validateSameEffect(others, "N+unit composition"))(
        others.map(_.r),
        others.map(_.e),
        List(TypeRepr.of[Unit])
      )(typeUnion)
    }

    private def fromMultiTypes(effectType: ZioEffectType)(rs: List[TypeRepr], es: List[TypeRepr], as: List[TypeRepr])(implicit typeUnion: TypeUnion) =
      ZioType(effectType)(
        effectType.composeRs(rs),
        effectType.composeEs(es)(typeUnion),
        effectType.composeAs(as)
      )

    def composeN(zioTypes: List[ZioType])(implicit typeUnion: TypeUnion): ZioType =
      val et = validateSameEffect(zioTypes, "N-composition")
      val (rs, es, as) = zioTypes.map(zt => (zt.r, zt.e, zt.a)).unzip3
      ZioType(validateSameEffect(zioTypes, "N-composition"))(et.composeRs(rs), et.composeEs(es), et.composeAs(as))

    private def validateSameEffect(types: List[ZioType], label: String): ZioEffectType = {
      types.map(_.effectType).reduce((a, b) => {
        if (a != b)
          report.errorAndAbort(s"Different effect types encountered: ${a.tpe.show} and ${b.tpe.show}")
        else
          a
      })
    }

    def compose(a: ZioType, b: ZioType)(implicit typeUnion: TypeUnion): ZioType =
      val et = validateSameEffect(List(a, b), "a/b composition")
      ZioType(et)(et.composeR(a.r, b.r), et.composeE(a.e, b.e), et.composeA(a.a, b.a))
  }

  // private class RunCall(effectType: ZioEffectType) {
  //   import Extractors._
  //   def unapply(tree: Tree): Option[Expr[_]] = {
  //     tree match
  //       case DottyExtensionCall(invocation @ DirectRunCallAnnotated.Term(), effect) =>
  //         if (effectType.isEffectOf(effect.tpe))
  //           Some(effect.asExpr)
  //         else
  //           None

  //       case _ => None
  //   }
  // }
}
