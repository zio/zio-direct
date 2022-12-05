package zio.direct.core.metaprog

import zio.direct.core.util.TraceType

case class Instructions(info: InfoBehavior, collect: Collect, verify: Verify, typeUnion: TypeUnion, traceTypes: List[TraceType]) {
  // For debugging purposes, check if there is any visibility setting enabled
  // to know whether to print various ad-hoc things.
  def anyVis = info != InfoBehavior.Silent
}

sealed trait InfoBehavior {
  def showComputedType: Boolean
  def showComputedTypeDetail: Boolean
  def showDeconstructed: Boolean
  def showReconstructed: Boolean
  def showReconstructedTree: Boolean
}
object InfoBehavior {
  case object Silent extends InfoBehavior {
    val showComputedType = false
    val showComputedTypeDetail = false
    val showDeconstructed = false
    val showReconstructed = false
    val showReconstructedTree = false
  }
  case object Info extends InfoBehavior {
    val showComputedType = true
    val showComputedTypeDetail = false
    val showDeconstructed = false
    val showReconstructed = true
    val showReconstructedTree = false
  }
  case object Verbose extends InfoBehavior {
    val showComputedType = true
    val showComputedTypeDetail = true
    val showDeconstructed = true
    val showReconstructed = true
    val showReconstructedTree = false
  }
  case object VerboseTree extends InfoBehavior {
    val showComputedType = true
    val showComputedTypeDetail = true
    val showDeconstructed = true
    val showReconstructed = true
    val showReconstructedTree = true
  }
  val default = Silent
}

sealed trait TypeUnion
object TypeUnion {
  case object OrType extends TypeUnion
  case object LeastUpper extends TypeUnion
  val default = OrType
}

sealed trait Verify
object Verify {
  case object Strict extends Verify
  case object Lenient extends Verify
  case object None extends Verify
  val default = Strict
}

sealed trait Collect
object Collect {
  case object Sequence extends Collect
  case object Parallel extends Collect
  val default = Sequence
}
