// package zio.direct.examples

// import zio.ZIO
// import zio.direct._
// import java.sql.Connection
// import java.io.InputStreamReader
// import java.io.IOException
// import java.sql.SQLException
// import zio.Scope
// import java.io.InputStream

// object ErrorHandlingExamples {

//   object ObjectModel {

//     object Database {
//       def openConnection(): ZIO[Scope, Throwable, Connection] = ???
//     }
//     object S3Object {
//       def openInputStream(path: String): ZIO[Scope, Throwable, InputStream] = ???
//     }

//     def handle(e: Exception): ZIO[Any, Nothing, Unit] = ???
//   }
//   import ObjectModel._

//   // def openFileReader(path: String) = defer {
//   //   val file = ZIO.fromAutoCloseable(ZIO.attempt(new FileInputStream(path))).run
//   //   new InputStreamReader(file)
//   // }

//   val out: ZIO[Scope, Throwable, Unit] = // // //
//     defer {
//       try {
//         unsafe {
//           val input = S3Object.openInputStream("foo/bar").run
//           // TODO write a test with this case and where InputStreamReader is written into a ValDef
//           new InputStreamReader(input)
//           val conn = Database.openConnection().run
//           val ps = conn.prepareStatement("INSERT ? INTO Someplace")
//           // ps.setClob(1, null)
//           ps.execute()
//         }
//       } catch {
//         case e: IOException  => handle(e).run
//         case e: SQLException => handle(e).run
//       }
//     }

// }
