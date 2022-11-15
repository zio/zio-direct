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
import zio.direct.core.util.ComputeTotalZioType
import zio.direct.core.metaprog.WithPrintIR
import zio.direct.core.metaprog.Embedder._
import zio.direct.core.norm.WithComputeType
import zio.direct.core.norm.WithReconstructTree
import zio.direct.core.norm.WithDecomposeTree
import zio.direct.core.util.ShowDetails

class Transformer(inputQuotes: Quotes)
    extends WithIR
    with WithComputeType
    with WithPrintIR
    with WithReconstructTree
    with WithDecomposeTree {

  implicit val macroQuotes = inputQuotes
  import quotes.reflect._

  def symbolLineage(sym: Symbol): Unit =
    if (sym.isNoSymbol) {
      ()
    } else {
      symbolLineage(sym.owner)
    }

  def apply[T: Type](valueRaw: Expr[T], instructions: Instructions): Expr[ZIO[?, ?, ?]] = {
    val value = valueRaw.asTerm.underlyingArgument

    // // Do a top-level transform to check that there are no invalid constructs
    if (instructions.verify != Verify.None)
      Allowed.validateBlocksIn(value.asExpr, instructions)
    // // Do the main transformation
    val transformedRaw = DecomposeTree.orPure(value)

    if (instructions.info.showDeconstructed)
      println("============== Deconstructed Instructions ==============\n" + PrintIR(transformedRaw))

    val transformed = WrapUnsafes(transformedRaw)
    val transformedSameAsRaw = transformed != transformedRaw
    if (instructions.info.showDeconstructed) {
      if (transformedSameAsRaw)
        println("============== Monadified Tries ==============\n" + PrintIR(transformed))
      else
        println("============== Monadified Tries (No Changes) ==============")
    }

    val output = ReconstructTree(instructions).fromIR(transformed)
    if (instructions.info.showReconstructed)
      val showDetailsMode =
        instructions.info match {
          case InfoBehavior.VerboseTree => ShowDetails.Verbose
          case InfoBehavior.Verbose     => ShowDetails.Standard
          case _                        => ShowDetails.Compact
        }
      println("============== Reconstituted Code ==============\n" + Format.Expr(output, Format.Mode.DottyColor(showDetailsMode)))

    if (instructions.info.showReconstructedTree)
      println("============== Reconstituted Code Raw ==============\n" + Format(Printer.TreeStructure.show(output.asTerm)))

    val computedType = ComputeType.fromIR(transformed)

    val zioType = computedType.toZioType

    if (instructions.info.showComputedTypeDetail)
      println(
        s"""-------------
        |Computed-Type: ${Format.TypeRepr(zioType)}
        |Discovered-Type: ${Format.TypeRepr(output.asTerm.tpe)}
        |Is Subtype: ${zioType <:< output.asTerm.tpe}
        |""".stripMargin
      )

    // // TODO verify that there are no await calls left. Otherwise throw an error
    val ownerPositionOpt = topLevelOwner.pos

    (computedType.r.asType, computedType.e.asType, computedType.a.asType) match {
      case ('[r], '[e], '[a]) =>
        val computedTypeMsg = s"Computed Type: ${Format.TypeOf[ZIO[r, e, a]]}"

        if (instructions.info.showComputedType)
          ownerPositionOpt match {
            case Some(pos) =>
              report.info(computedTypeMsg, pos)
            case None =>
              report.info(computedTypeMsg)
          }
        '{ $output.asInstanceOf[ZIO[r, e, a]] }
    }
  }
}
