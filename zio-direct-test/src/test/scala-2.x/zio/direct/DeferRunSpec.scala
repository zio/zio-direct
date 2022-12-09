// package zio.direct

// import zio._
// import zio.test._
// import zio.direct.core.testing.TestSupport

// trait DeferRunSpec extends ZIOSpecDefault with TestSupport {
//   // various config parameters to test zio dependency
//   case class ConfigInt(value: Int)
//   case class ConfigString(value: String)
//   case class ConfigBool(value: Boolean)
//   case class ConfigT[T](value: T)
//   case class ConfigList(value: List[Int])

//   class FooError extends Exception("foo")
//   def throwFoo() = throw new FooError
//   def makeFooError = new FooError

//   def wantStringThrowFoo(): String = throw new FooError

//   class BarError extends Exception("foo")
//   def throwBar() = throw new BarError
//   def makeBarError = new BarError

//   implicit class TestZIOOps(result: ZIO[Any, _, TestResult]) {
//     def andAssert(otherResult: TestResult) =
//       for {
//         r <- result
//       } yield r && otherResult
//   }

//   implicit class TestZIOOps2(result: TestResult) {
//     def andAssert(otherResult: ZIO[Any, _, TestResult]) =
//       for {
//         r <- otherResult
//       } yield result && r
//   }

//   val errorMsg =
//     "Detected an `run` call inside of an unsupported structure"

//   // transparent inline def isType[T](inline input: Any)
//   // transparent inline def assertIsType[T](inline input: Any)
//   // inline def runLiftTest[T](expected: T)(inline body: T)
//   // inline def runLiftTestLenient[T](expected: T)(inline body: T)
//   // transparent inline def runLiftFailLenientMsg(errorStringContains: String)(body: String)
//   // transparent inline def runLiftFailMsg(errorStringContains: String)(body: String)
//   // inline def sourceLocation: SourceLocation = ${ DeferRunSpec.sourceLocationImpl }
//   // transparent inline def runLiftFail(body: String)
// }
