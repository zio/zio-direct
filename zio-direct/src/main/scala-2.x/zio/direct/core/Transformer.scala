package zio.direct.core

import zio.direct.core.metaprog.WithIR
import zio.direct.core.metaprog.WithPrintIR
import zio.direct.core.norm.WithDecomposeTree
import zio.direct.core.util.WithInterpolator
import zio.direct.core.metaprog.WithZioType
import zio.direct.core.util.WithFormat
import zio.direct.core.util.WithUnsupported
import zio.direct.core.metaprog.Instructions
import zio.direct.core.util.Announce
import zio.direct.core.norm.WithComputeType
import zio.direct.core.norm.WithReconstructTree
import zio.direct.core.metaprog.InfoBehavior
import scala.reflect.internal.util.RangePosition

abstract class Transformer
    extends WithIR
    // with WithComputeType
    with WithPrintIR
    // with WithReconstructTree
    with WithDecomposeTree
    with WithInterpolator
    with WithZioType
    with WithFormat
    with WithUnsupported
    with WithComputeType
    with WithReconstructTree {

  import c.universe._

  protected def posFileStr(pos: Position): String = {
    val path = pos.source.path
    s"$path:${pos.line}:${pos.column}"
  }

  private def deconstructAndAnncounce(value: Tree, instructions: Instructions) = {
    // // Do a top-level transform to check that there are no invalid constructs
    // if (instructions.verify != Verify.None)
    //   Allowed.validateBlocksIn(value.asExpr, instructions)

    // // Do the main transformation
    val transformedRaw = Decompose(instructions)(value)
    def sourceFile = Announce.FileShow.FullPath(posFileStr(value.pos))

    if (instructions.info != InfoBehavior.Silent)
      Announce.section("TRANSFORMING AT", "", sourceFile, Announce.Volume.Loud)

    if (instructions.info.showDeconstructed)
      Announce.section("Deconstructed Instructions", PrintIR(transformedRaw), sourceFile)

    (transformedRaw, sourceFile)
  }

  private def findEncosingOwner = {
    // object Util {
    //   def isSynthetic(s: Symbol) = isSyntheticName(getName(s))
    //   def isSyntheticName(name: String) = {
    //     name == "<init>" || (name.startsWith("<local ") && name.endsWith(">")) || name == "$anonfun"
    //   }
    //   def getName(s: Symbol) = s.name.decodedName.toString.trim
    // }

    // val enclosingOwner = c.internal.enclosingOwner
    // println(s"============= Enclosing Owner: ${show(enclosingOwner.pos)}")

    val owner = c.internal.enclosingOwner
    // while (Util.isSynthetic(owner)) owner = owner.owner

    // val o = owner.pos.removeElement(c.enclosingPosition.pos)

    println(s"============= Enclosing Owner: ${showRaw(owner.pos)} is Range: ${owner.pos.focus.start} -> ${owner.pos.focus.end}")

    if (owner != NoSymbol)
      Some(owner.pos.focus)
    else
      None
  }

  private def wrapUnsafes(ir: IR, sourceFile: Announce.FileShow.FullPath, instructions: Instructions) = {
    val wrappedIR = WrapUnsafes(ir)
    val transformedSameAsRaw = wrappedIR != ir
    if (instructions.info.showDeconstructed) {
      if (transformedSameAsRaw)
        Announce.section("Monadified Tries", PrintIR(wrappedIR), sourceFile)
      else
        Announce.section("Monadified Tries (No Changes)", "", sourceFile)
    }
    wrappedIR
  }

  private def reconstructTree(ir: IR, sourceFile: Announce.FileShow.FullPath, instructions: Instructions) = {
    val outputRaw = ReconstructTree(instructions).fromIR(ir)
    import org.scalamacros.resetallattrs._
    val output = c.resetAllAttrs(outputRaw)
    if (instructions.info.showReconstructed)
      Announce.section("Reconstituted Code", Format(Format.Term(output)), sourceFile)
    output
  }

  def apply[T](value: Tree, instructions: Instructions): Tree = {
    val (ir, sourceFile) = deconstructAndAnncounce(value, instructions)

    val computedType = ComputeType.fromIR(ir)(instructions).toZioType
    val ownerPositionOpt = findEncosingOwner
    val wrappedIR = wrapUnsafes(ir, sourceFile, instructions)
    val output = reconstructTree(wrappedIR, sourceFile, instructions)

    def showEnclosingType() = {
      val computedTypeMsg = s"Computed Type: ${Format.Type(computedType)}"
      if (instructions.info.showComputedType)
        ownerPositionOpt match {
          case Some(pos) =>
            report.info(computedTypeMsg, pos)
          case None =>
            report.info(computedTypeMsg)
        }
    }

    showEnclosingType()
    c.typecheck(q"$output.asInstanceOf[$computedType]")
  }
}
