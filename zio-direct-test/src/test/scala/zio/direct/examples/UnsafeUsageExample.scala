// package zio.direct

// import zio._
// import zio.ZIO.{unsafe => _}
// import java.sql.SQLException
// import java.io.IOException
// import java.io.InputStreamReader
// import java.sql.Connection
// import java.io.InputStream

// object UnsafeUsageExample {

//   {
//     object ObjectModel {

//       object Database {
//         def openConnection(): ZIO[Scope, Throwable, Connection] = ???
//       }
//       object S3Object {
//         def openInputStream(path: String): ZIO[Scope, Throwable, InputStream] = ???
//       }

//       def handle(e: Exception): ZIO[Any, Nothing, Unit] = ???
//     }
//     import ObjectModel._

//     defer {
//       try {
//         unsafe {
//           val input = S3Object.openInputStream("foo/bar").run
//           // This is not wrapped in an attempt need to do that
//           val reader = new InputStreamReader(input)
//           val conn = Database.openConnection().run
//           val ps = conn.prepareStatement("INSERT ? INTO Someplace")
//           ps.setClob(1, reader)
//           ps.execute()
//         }
//       } catch {
//         case e: IOException  => handle(e).run
//         case e: SQLException => handle(e).run
//       }
//     }
//   }

// }
