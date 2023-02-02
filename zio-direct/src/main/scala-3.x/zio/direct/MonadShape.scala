package zio.direct

sealed trait MonadShape
object MonadShape {
  sealed trait Variances
  trait Variances1[T1 <: Variance] extends Variances
  trait Variances2[T1 <: Variance, T2 <: Variance] extends Variances
  trait Variances3[T1, T2, T3] extends Variances
  trait Variances4[T1 <: Variance, T2 <: Variance, T3 <: Variance, T4 <: Variance] extends Variances
  trait Variances5[T1 <: Variance, T2 <: Variance, T3 <: Variance, T4 <: Variance, T5 <: Variance] extends Variances
  trait Variances6[T1 <: Variance, T2 <: Variance, T3 <: Variance, T4 <: Variance, T5 <: Variance, T6 <: Variance] extends Variances
  trait Variances7[T1 <: Variance, T2 <: Variance, T3 <: Variance, T4 <: Variance, T5 <: Variance, T6 <: Variance, T7 <: Variance] extends Variances

  sealed trait Letters
  trait Letters1[T1 <: Letter] extends Letters
  trait Letters2[T1 <: Letter, T2 <: Letter] extends Letters
  trait Letters3[T1, T2, T3] extends Letters
  trait Letters4[T1 <: Letter, T2 <: Letter, T3 <: Letter, T4 <: Letter] extends Letters
  trait Letters5[T1 <: Letter, T2 <: Letter, T3 <: Letter, T4 <: Letter, T5 <: Letter] extends Letters
  trait Letters6[T1 <: Letter, T2 <: Letter, T3 <: Letter, T4 <: Letter, T5 <: Letter, T6 <: Letter] extends Letters
  trait Letters7[T1 <: Letter, T2 <: Letter, T3 <: Letter, T4 <: Letter, T5 <: Letter, T6 <: Letter, T7 <: Letter] extends Letters

  sealed trait Variance
  object Variance {
    trait Covariant extends Variance
    case object Covariant extends Covariant
    trait Contravariant extends Variance
    case object Contravariant extends Contravariant
    trait Unused extends Variance
    case object Unused extends Unused
  }

  sealed trait Letter
  object Letter {
    trait R extends Letter
    case object R extends R
    trait E extends Letter
    case object E extends E
    trait A extends Letter
    case object A extends A
    trait Other extends Letter
    case object Other extends Other
  }
}
