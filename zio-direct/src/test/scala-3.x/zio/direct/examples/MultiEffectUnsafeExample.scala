package zio.direct.examples

import zio._
import zio.direct._
import java.sql.Connection
import java.io.InputStream
import java.io.IOException
import java.sql.SQLException
import java.io.InputStreamReader

object MultiEffectUnsafeExample {

  // TODO test that scoping propagation works right
  // see ErrorHandlingExamples for more detail about that

  {
    object ObjectModel {

      object Database {
        def openConnection(): ZIO[Scope, Throwable, Connection] = ???
      }
      object S3Object {
        def openInputStream(path: String): ZIO[Scope, Throwable, InputStream] = ???
      }

      def handle(e: Exception): ZIO[Any, Nothing, Unit] = ???
    }
    import ObjectModel._

    defer {
      try {
        unsafe {
          val input = S3Object.openInputStream("foo/bar").run
          // This is not wrapped in an attempt need to do that
          val reader = InputStreamReader(input)
          val conn = Database.openConnection().run
          val ps = conn.prepareStatement("INSERT ? INTO Someplace")
          ps.setClob(1, reader)
          ps.execute()
        }
      } catch {
        case e: IOException  => handle(e).run
        case e: SQLException => handle(e).run
      }
    }
  }

}
