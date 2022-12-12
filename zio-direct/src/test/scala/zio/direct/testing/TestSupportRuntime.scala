package zio.direct.testing

import zio.test._
import scala.reflect.runtime.universe._

private[direct] trait TestSupportRuntime {

  def isTypeRuntime[T, I](input: I)(implicit tt: TypeTag[T], ti: TypeTag[I]): Boolean =
    tt.tpe =:= ti.tpe

  def assertIsTypeRuntime[T, I](input: I)(implicit tt: TypeTag[T], ti: TypeTag[I]): TestResult =
    assertTrue(isTypeRuntime[T, I](input))
}
