// package zio.direct.examples

// import zio.ZIO
// import zio.direct._
// import java.io.IOException

// object IntroductionExamples {

//   /**
//    * **************************** Sequence *************************************
//    */
//   object Sequence {
//     object ImperativeObjectModel {
//       class File {}
//       def read(file: File): String = ???
//       def write(file: File, content: String): Unit = ???
//       def fileA: File = ???
//       def fileB: File = ???
//       def fileC: File = ???
//     }
//     {
//       import ImperativeObjectModel._
//       val textA = read(fileA)
//       val textB = read(fileB)
//       write(fileC, textA + textB)
//     }

//     object FunctionalObjectModel {
//       class File {}
//       def read(file: File): ZIO[Any, Throwable, String] = ???
//       def write(file: File, content: String): ZIO[Any, Throwable, Unit] = ???
//       def fileA: File = ???
//       def fileB: File = ???
//       def fileC: File = ???

//       // regular way
//       {
//         read(fileA).flatMap { textA =>
//           read(fileB).flatMap { textB =>
//             write(fileC, textA + textB)
//           }
//         }
//       }
//       // for-comprehension way
//       {
//         for {
//           textA <- read(fileA)
//           textB <- read(fileB)
//           _ <- write(fileC, textA + textB)
//         } yield ()
//       }
//       // using deferA
//       {
//         defer {
//           val textA = run(read(fileA))
//           val textB = run(read(fileB))
//           run(write(fileC, textA + textB))
//         }
//       }

//       // var x = 123
//       // PrintMac(x + 1) // // //∂
//       // defer: Op => ZIO[E, R, Op]
//       // run: ZIO[E, R, Op] => Op
//       // explain how .run is same as run(...)
//       // using deferB
//       {
//         defer {
//           val textA = read(fileA).run
//           val textB = read(fileB).run
//           write(fileC, textA + textB).run
//         }
//       }
//     }
//   }

//   /**
//    * **************************** IF *************************************
//    */
//   object If {
//     {
//       object ImperativeObjectModel {
//         class Row {}
//         class Database {
//           def transactionsEnabled(): Boolean = ???
//           def lazyFetchEnabled(): Boolean = ???
//           def bulkInsert(row: List[Row]): Unit = ???
//         }
//         object Database {
//           def open(): Database = ???
//         }
//         def waitT(): Unit = ()
//       }
//       import ImperativeObjectModel._

//       // original imperetive code
//       {
//         val rows: List[Row] = ???
//         val db = Database.open()
//         if (db.transactionsEnabled() && db.lazyFetchEnabled()) {
//           db.bulkInsert(rows)
//         }
//       }
//     }

//     {
//       object FunctionalObjectModel {
//         class Row {}
//         class Database {
//           def transactionsEnabled(): ZIO[Any, Throwable, Boolean] = ???
//           def lazyFetchEnabled(): ZIO[Any, Throwable, Boolean] = ???
//           def bulkInsert(row: List[Row]): ZIO[Any, Throwable, Unit] = ???
//         }
//         object Database {
//           def open(): ZIO[Any, Throwable, Database] = ???
//         }
//         def doSomethingWith(row: Row): Unit = ???
//         def waitT(): Unit = ()
//         def rows: List[Row] = ???
//       }

//       {
//         import FunctionalObjectModel._
//         for {
//           db <- Database.open()
//           te <- db.transactionsEnabled()
//           lf <- db.lazyFetchEnabled()
//           _ <- {
//             if (lf)
//               db.bulkInsert(rows)
//             else
//               ZIO.unit
//           }
//         } yield ()
//       }

//       import FunctionalObjectModel._
//       // Functional code
//       {
//         val rows: List[Row] = ???
//         Database.open().flatMap { db =>
//           db.transactionsEnabled().flatMap { te =>
//             if (te)
//               db.lazyFetchEnabled().flatMap { lf =>
//                 if (lf)
//                   db.bulkInsert(rows)
//                 else
//                   ZIO.unit
//               }
//             else
//               ZIO.unit
//           }
//         }
//       }
//       // Defer code
//       {
//         val rows: List[Row] = ???
//         defer {
//           val db = Database.open().run
//           if (
//             db.transactionsEnabled().run &&
//             db.lazyFetchEnabled().run
//           ) {
//             db.bulkInsert(rows).run
//           }
//         }
//       }
//     }
//   }

//   /**
//    * **************************** Exceptions *************************************
//    */
//   object Exceptions {
//     object ImperetiveObjectModel {
//       class Json {}
//       class JsonFile {
//         def readToJson(): Json = ???
//         def close(): Unit = ???
//       }
//       object JsonFile {
//         def open(path: String): JsonFile = ???
//       }
//       def path: String = ???

//       class DecodingException extends Exception("Decoding Error")
//       def handleIO(e: Exception): Unit = ???
//       def handleDE(e: Exception): Unit = ???
//     }
//     {
//       import ImperetiveObjectModel._

//       var file: JsonFile = null
//       try {
//         file = JsonFile.open(path)
//         file.readToJson()
//       } catch {
//         case e: IOException       => handleIO(e)
//         case e: DecodingException => handleDE(e)
//       } finally {
//         file.close()
//       }
//     }

//     object FunctionalObjectModel {
//       class Json {}
//       class JsonFile {
//         def readToJson(): ZIO[Any, Throwable, Json] = ???
//         def close(): ZIO[Any, Throwable, Unit] = ???
//       }
//       object JsonFile {
//         def from(path: String): JsonFile = ???
//       }
//       def path: String = ???

//       class DecodingException extends Exception("Decoding Error")
//       def handleIO(e: Exception): ZIO[Any, Throwable, Unit] = ???
//       def handleDE(e: Exception): ZIO[Any, Throwable, Unit] = ???
//     }

//     {
//       import FunctionalObjectModel._

//       ZIO.succeed(JsonFile.from(path)).flatMap { jsonFile =>
//         jsonFile.readToJson()
//           .catchSome {
//             case e: IOException       => handleIO(e)
//             case e: DecodingException => handleDE(e)
//           }.ensuring {
//             jsonFile.close().orDie
//           }
//       }
//     }
//     // Using Defer
//     {
//       import FunctionalObjectModel._

//       defer {
//         val file = JsonFile.from(path)
//         try {
//           file.readToJson().run
//         } catch {
//           case e: IOException       => handleIO(e).run
//           case e: DecodingException => handleDE(e).run
//         } finally {
//           file.close().run
//         }
//       }
//     }

//   }

//   /**
//    * **************************** WHILE *************************************
//    */
//   object While {

//     object ImperetiveObjectModel {
//       class File {
//         def readLine(): String = ???
//         def close(): Unit = ???
//       }
//       def makeFile(): File = ???
//     }

//     {
//       import ImperetiveObjectModel._
//       val file: File = makeFile()
//       val buffer = new StringBuffer()

//       var line: String = file.readLine()
//       while (line != null) {
//         buffer.append(line)
//         line = file.readLine()
//       }

//       file.close()
//     }

//     // More similar to functional case
//     {
//       import ImperetiveObjectModel._
//       val file: File = makeFile()
//       val buffer = new StringBuffer()

//       var line: String = file.readLine()

//       def whileFun(): Unit =
//         if (line != null) {
//           buffer.append(line)
//           line = file.readLine()
//           whileFun()
//         } else {
//           ()
//         }

//       file.close()
//     }

//     object FunctionalObjectModel {
//       class File {
//         def readLine(): ZIO[Any, Throwable, String] = ???
//         def close(): ZIO[Any, Throwable, Unit] = ???
//       }
//       def makeFile(): ZIO[Any, Throwable, File] = ???
//     }

//     {
//       import FunctionalObjectModel._
//       makeFile().flatMap { file =>
//         val buffer = new StringBuffer()

//         defer.use(Use.withLenientCheck) {
//           val line0 = file.readLine().run
//           var line: String = line0
//           while (line != null) {
//             buffer.append(line)
//             val lineN = file.readLine().run
//             line = lineN
//           }
//         }
//       }
//     }

//     {
//       import FunctionalObjectModel._
//       makeFile().flatMap { file =>
//         val buffer = new StringBuffer()

//         file.readLine().flatMap { line0 =>
//           var line = line0
//           def whileFun(): ZIO[Any, Throwable, Unit] =
//             if (line != null) {
//               buffer.append(line)
//               file.readLine().flatMap { lineN =>
//                 line = lineN
//                 whileFun()
//               }
//             } else
//               ZIO.unit
//           whileFun()
//         }
//       }
//     }

//     {
//       import FunctionalObjectModel._

//       makeFile().flatMap { file =>
//         val buffer = new StringBuffer()
//         defer.use(Use.withLenientCheck) {
//           val line0 = file.readLine().run
//           var line: String = line0
//           while (line != null) {
//             buffer.append(line)
//             val lineN = file.readLine().run
//             line = lineN
//           }
//         }
//       }
//     }
//   }

//   /**
//    * **************************** Foreach *************************************
//    */
//   object Foreach {
//     object ImperativeObjectModel {
//       def postUpdate(url: String): String = ???
//       def getWebsites(): List[String] = ???
//     }
//     {
//       import ImperativeObjectModel._
//       for (site <- getWebsites()) {
//         postUpdate(site)
//       }
//     }

//     object FunctionalObjectModel {
//       def postUpdate(url: String): ZIO[Any, Throwable, String] = ???
//       def getWebsites(): ZIO[Any, Throwable, List[String]] = ???
//     }
//     // using regular functional
//     {
//       import FunctionalObjectModel._
//       getWebsites()
//         .flatMap { websites =>
//           ZIO.foreach(websites)(
//             postUpdate(_)
//           )
//         }
//     }
//     // using defer
//     {
//       import FunctionalObjectModel._
//       defer {
//         for (site <- getWebsites().run) {
//           postUpdate(site).run
//         }
//       }
//     }
//   }

// }
