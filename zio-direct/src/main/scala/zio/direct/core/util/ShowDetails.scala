package zio.direct.core.util

trait ShowDetails {
  def showImplicitFunctionParams: Boolean
  def showImplicitClauses: Boolean
  def showBoundsTypes: Boolean
  def showTypeParams: Boolean
  def showAsInstanceOf: Boolean
}

object ShowDetails {
  object Compact extends ShowDetails {
    def showImplicitFunctionParams: Boolean = false
    def showImplicitClauses: Boolean = false
    def showBoundsTypes: Boolean = false
    def showTypeParams: Boolean = false
    def showAsInstanceOf: Boolean = false
  }
  object Standard extends ShowDetails {
    def showImplicitFunctionParams: Boolean = false
    def showImplicitClauses: Boolean = false
    def showBoundsTypes: Boolean = false
    def showTypeParams: Boolean = false
    def showAsInstanceOf: Boolean = true
  }
  object Verbose extends ShowDetails {
    def showImplicitFunctionParams: Boolean = true
    def showImplicitClauses: Boolean = true
    def showBoundsTypes: Boolean = true
    def showTypeParams: Boolean = true
    def showAsInstanceOf: Boolean = true
  }
}
