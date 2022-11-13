package zio.asyncawait.core

class NotDeferredException(msg: String) extends Exception(msg)

object NotDeferredException {
  def apply() =
    throw new NotDeferredException("The statement can only be used inside of a `defer { ... }` block.")
  def fromNamed(name: String) =
    throw new NotDeferredException(s"The expression `$name` be used inside of a `defer { ... }` block.")
}