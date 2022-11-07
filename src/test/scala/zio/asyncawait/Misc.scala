package zio.asyncawait

// import zio._

// object Misc {

//   class Session(input: String)
//   class Repo(session: Session) {
//     def doSomething: String = "blah"
//     def doSomethingElse: ZIO[Session, Nothing, String] = ???
//   }
//   val hasRepo: ZIO[Session, Nothing, Repo] = ???
//   val makeRepo = ZLayer.fromFunction(Repo(_))
//   val somethingElseService = ZIO.serviceWithZIO[Repo](_.doSomethingElse)
//   val vvv = makeRepo.apply(hasRepo)

//   //vvv.provideLayer()
//   // implicit class ZioExt[R, E, A](zio: ZIO[R, E, A])(implicit trace: Trace) {
//   //   def provideJust[D]
//   // }


//   def main(args: Array[String]): Unit = {



//   }
// }

case class Config(value: String)