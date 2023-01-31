package zio.direct.core

import scala.quoted._
import zio.direct.core.metaprog.Extractors._
import zio.direct.core.metaprog._
import zio.direct._
import zio.direct.core.util.Format
import scala.collection.mutable
import zio.Chunk
import zio.direct.core.util.PureTree
import zio.direct.core.util.WithInterpolator
import zio.direct.core.util.Unsupported
import zio.direct.core.metaprog.WithPrintIR
import zio.direct.core.metaprog.Embedder._
import zio.direct.core.norm.WithComputeType
import zio.direct.core.norm.WithReconstructTree
import zio.direct.core.norm.WithDecomposeTree
import zio.direct.core.norm.WithResolver
import zio.direct.core.util.ShowDetails
import zio.direct.Internal.deferred
import zio.direct.core.util.Announce
import zio.direct.Internal.Marker

class Transformer[F[_, _, _]: Type, F_out: Type](inputQuotes: Quotes)
    extends WithF
    with WithIR
    with WithComputeType
    with WithPrintIR
    with WithReconstructTree
    with WithDecomposeTree
    with WithInterpolator
    with WithZioType
    with WithResolver {

  implicit val macroQuotes = inputQuotes
  import quotes.reflect._

  protected def posFileStr(pos: Position): String =
    val path = pos.sourceFile.path
    s"$path:${pos.startLine + 1}:${pos.startColumn}"

  def apply[T: Type](valueRaw: Expr[T], instructions: Instructions): Expr[F_out] = {
    val value = valueRaw.asTerm.underlyingArgument

    val effectType = ZioEffectType.of[F]
    val directMonad = DirectMonad.of[F]

    // Do a top-level transform to check that there are no invalid constructs
    if (instructions.verify != Verify.None)
      Allowed.validateBlocksIn(instructions, effectType.isEffectOf)(value.asExpr)

    // // Do the main transformation
    val transformedRaw = Decompose[F](directMonad, effectType, instructions).apply(value)

    def fileShow = Announce.FileShow.FullPath(posFileStr(valueRaw.asTerm.pos))

    if (instructions.info != InfoBehavior.Silent)
      Announce.section("TRANSFORMING AT", "", fileShow, Announce.Volume.Loud)

    if (instructions.info.showDeconstructed)
      Announce.section("Deconstructed Instructions", PrintIR(transformedRaw), fileShow)

    val transformed = WrapUnsafes[F](directMonad).apply(transformedRaw)
    val transformedSameAsRaw = transformed != transformedRaw
    if (instructions.info.showDeconstructed) {
      if (transformedSameAsRaw)
        Announce.section("Monadified Tries", PrintIR(transformed), fileShow)
      else
        Announce.section("Monadified Tries (No Changes)", "", fileShow)
    }

    val irt = ComputeIRT(effectType, instructions.typeUnion)(transformed)
    val output = ReconstructTree[F](directMonad, effectType, instructions).fromIR(irt)
    if (instructions.info.showReconstructed)
      val showDetailsMode =
        instructions.info match {
          case InfoBehavior.VerboseTree => ShowDetails.Verbose
          case InfoBehavior.Verbose     => ShowDetails.Standard
          case _                        => ShowDetails.Compact
        }
      Announce.section("Reconstituted Code", Format.Term(output, Format.Mode.DottyColor(showDetailsMode)), fileShow)

    if (instructions.info.showReconstructedTree)
      Announce.section("Reconstituted Code Raw", Format(Printer.TreeStructure.show(output)), fileShow)

    val computedType = irt.zpe
    val zioType = computedType.toZioType

    if (instructions.info.showComputedTypeDetail)
      println(
        s"""-------------
        |Computed-Type: ${Format.TypeRepr(zioType)}
        |Discovered-Type: ${Format.TypeRepr(output.tpe)}
        |Is Subtype: ${zioType <:< output.tpe}
        |""".stripMargin
      )

    // // TODO verify that there are no run calls left. Otherwise throw an error
    val ownerPositionOpt = topLevelOwner.pos

    // If there are any remaining run-calls in the tree then fail
    // TODO need to figure out a way to test this
    Allowed.finalValidityCheck(instructions, effectType.isEffectOf)(output.asExpr)

    computedType.asTypeTuple match {
      case ('[r], '[e], '[a]) =>
        val computedTypeMsg = s"Computed Type: ${Format.TypeOf[F[r, e, a]]}"

        if (instructions.info.showComputedType)
          ownerPositionOpt match {
            case Some(pos) =>
              report.info(computedTypeMsg, pos)
            case None =>
              report.info(computedTypeMsg)
          }
        // '{ deferred[zio.ZIO, r, e, a](${ output.asExpr }.asInstanceOf[zio.ZIO[r, e, a]]) }.asExprOf[F_out]
        val deferredOutput =
          // '{ deferred[zio.ZIO[r, e, a]](${ output.asExpr }.asInstanceOf[zio.ZIO[r, e, a]]) }.asExprOf[F_out]
          '{ deferred(${ output.asExpr }.asInstanceOf[F[r, e, a]]) }.asExprOf[F_out]

        // Announce.section("Final Output Code", Format.Expr(deferredOutput, Format.Mode.DottyColor(ShowDetails.Verbose)), fileShow)
        deferredOutput
    }
  }
}
