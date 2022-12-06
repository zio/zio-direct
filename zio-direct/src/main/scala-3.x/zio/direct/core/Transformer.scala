package zio.direct.core

import scala.quoted._
import zio.Task
import zio.direct.core.metaprog.Extractors._
import zio.direct.core.metaprog._
import zio.direct._
import zio.direct.core.util.Format
import zio.ZIO
import scala.collection.mutable
import zio.Chunk
import zio.direct.core.util.PureTree
import zio.direct.core.util.WithInterpolator
import zio.direct.core.metaprog.WithPrintIR
import zio.direct.core.metaprog.Embedder._
import zio.direct.core.norm.WithComputeType
import zio.direct.core.norm.WithReconstructTree
import zio.direct.core.norm.WithDecomposeTree
import zio.direct.core.util.ShowDetails
import zio.direct.Dsl.Internal.deferred

class Transformer(inputQuotes: Quotes)
    extends WithIR
    with WithComputeType
    with WithPrintIR
    with WithReconstructTree
    with WithDecomposeTree
    with WithInterpolator
    with WithZioType {

  implicit val macroQuotes = inputQuotes
  import quotes.reflect._

  protected def posFileStr(pos: Position): String =
    val path = pos.sourceFile.path
    s"$path:${pos.startLine + 1}:${pos.startColumn}"

  def apply[T: Type](valueRaw: Expr[T], instructions: Instructions): Expr[ZIO[?, ?, ?]] = {
    val value = valueRaw.asTerm.underlyingArgument

    // // Do a top-level transform to check that there are no invalid constructs
    if (instructions.verify != Verify.None)
      Allowed.validateBlocksIn(value.asExpr, instructions)
    // // Do the main transformation
    val transformedRaw = Decompose(instructions)(value)

    def fileShow = FileShow.FullPath(posFileStr(valueRaw.asTerm.pos))

    if (instructions.info != InfoBehavior.Silent)
      Announce.section("TRANSFORMING AT", "", fileShow, Volume.Loud)

    if (instructions.info.showDeconstructed)
      Announce.section("Deconstructed Instructions", PrintIR(transformedRaw), fileShow)

    val transformed = WrapUnsafes(transformedRaw)
    val transformedSameAsRaw = transformed != transformedRaw
    if (instructions.info.showDeconstructed) {
      if (transformedSameAsRaw)
        Announce.section("Monadified Tries", PrintIR(transformed), fileShow)
      else
        Announce.section("Monadified Tries (No Changes)", fileShow)
    }

    val output = ReconstructTree(instructions).fromIR(transformed)
    if (instructions.info.showReconstructed)
      val showDetailsMode =
        instructions.info match {
          case InfoBehavior.VerboseTree => ShowDetails.Verbose
          case InfoBehavior.Verbose     => ShowDetails.Standard
          case _                        => ShowDetails.Compact
        }
      Announce.section("Reconstituted Code", Format.Expr(output, Format.Mode.DottyColor(showDetailsMode)), fileShow)

    if (instructions.info.showReconstructedTree)
      Announce.section("Reconstituted Code Raw", Format(Printer.TreeStructure.show(output.asTerm)), fileShow)

    val computedType = ComputeType.fromIR(transformed)(using instructions)

    val zioType = computedType.toZioType

    if (instructions.info.showComputedTypeDetail)
      println(
        s"""-------------
        |Computed-Type: ${Format.TypeRepr(zioType)}
        |Discovered-Type: ${Format.TypeRepr(output.asTerm.tpe)}
        |Is Subtype: ${zioType <:< output.asTerm.tpe}
        |""".stripMargin
      )

    // // TODO verify that there are no run calls left. Otherwise throw an error
    val ownerPositionOpt = topLevelOwner.pos

    // If there are any remaining run-calls in the tree then fail
    // TODO need to figure out a way to test this
    Allowed.finalValidtyCheck(output, instructions)

    computedType.asTypeTuple match {
      case ('[r], '[e], '[a]) =>
        val computedTypeMsg = s"Computed Type: ${Format.TypeOf[ZIO[r, e, a]]}" // /

        if (instructions.info.showComputedType)
          ownerPositionOpt match {
            case Some(pos) =>
              report.info(computedTypeMsg, pos)
            case None =>
              report.info(computedTypeMsg)
          }
        '{ deferred($output.asInstanceOf[ZIO[r, e, a]]) }
    }
  }
}
