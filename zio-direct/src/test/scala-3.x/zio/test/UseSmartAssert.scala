package zio.test

import zio.internal.stacktracer.SourceLocation

object UseSmartAssert {
  def of[A](
      expr: => A,
      codeString: Option[String] = None,
      assertionString: Option[String] = None
  )(
      assertion: Assertion[A]
  )(sourceLocation: SourceLocation) =
    Assertion.smartAssert(expr, codeString, assertionString)(assertion)(using sourceLocation)
}
