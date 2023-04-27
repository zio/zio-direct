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
import zio.direct.core.metaprog.WithAllowed
import zio.direct.core.metaprog.Verify
import zio.direct.core.metaprog.compat.WithAllowedCompat

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
    with WithReconstructTree
    with WithAllowed
    with WithAllowedCompat {

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
    val owner = c.internal.enclosingOwner
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

    // Force a manual "untyping" of all the identifiers because we change "val x = monad.run" into monad.flatMap(x => ...)
    // and Scala2 doesn't like the "role" of the identifier being changed for reasons that I do not fully understand.
    // Basically the idntifier being used in further in here: "val x = monad.run; <...>" is somehow not the same
    // as the new "x =>" that we create in "monad.flatMap(x =>" so that information needs to be reset in some kind of way.
    // Also importing 'org.scalamacros.resetallattrs' does the trick but that causes other problems e.g.
    // if there are any "import foo.{bar => baz}" in files using the defer macro, it will not remember that baz is an alias for bar.
    // This was encountered in unit tests where `zio.direct.{run => runBlock}` was done to avoid conflicts in the test
    // code because the ZIO base spec ZIOSpecDefault also has a `run` method in the class which would always take
    // precedence over the `run` method in the ZIO package.
    // Now we want to only reset identifiers that represent vals, not ones that represent defs, class-names etc...
    // since resetting the latter can make the tree invalid.
    val outputRetyped =
      zio.direct.core.metaprog.Trees.Transform(c)(outputRaw) {
        case id: Ident if (id.symbol != NoSymbol && id.symbol.isTerm && id.symbol.asTerm.isVal) => q"${id.name.toTermName}"
      }

    val output = c.untypecheck(outputRetyped) // c.resetAllAttrs(outputRaw)
    if (instructions.info.showReconstructed)
      Announce.section("Reconstituted Code", Format(Format.Term(output)), sourceFile)

    // Typecheck before returning since we will rely on the types later.
    // TODO since we are typechecking here, maybe doing it in ZioType is not needed anymore, should look into that.
    c.typecheck(output)
  }

  def apply[T](value: Tree, instructions: Instructions): Tree = {
    // Do a top-level transform to check that there are no invalid constructs
    if (instructions.verify != Verify.None)
      Allowed.validateBlocksIn(value, instructions)

    val (ir, sourceFile) = deconstructAndAnncounce(value, instructions)

    val ownerPositionOpt = findEncosingOwner
    val wrappedIR = wrapUnsafes(ir, sourceFile, instructions)
    val computedType = ComputeType.fromIR(wrappedIR)(instructions).zpe.toZioType
    val output = reconstructTree(wrappedIR, sourceFile, instructions)

    Allowed.finalValidtyCheck(output, instructions)

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
    c.typecheck(q"zio.direct.Internal.deferred($output).asInstanceOf[$computedType]")
  }
}
