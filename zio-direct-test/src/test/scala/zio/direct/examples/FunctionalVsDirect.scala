// package zio.direct.examples

// import zio.ZIO
// import zio.direct._

// object CorrectnessExamples {

//   class Boom extends Exception("Boom!")
//   object Boom {
//     def apply() = new Boom()
//   }
//   def random(): Boolean = true

//   /**
//    * **************************** WHILE EXAMPLES *************************************
//    */
//   object While {
//     {
//       object ImperativeObjectModel {
//         class Row {}
//         class Database {
//           def nextRow(): Row = ???
//           def hasNextRow(): Boolean = ???
//           def lockNextRow(): Boolean = ???
//         }
//         object Database {
//           def open: Database = ???
//         }
//         def doSomethingWith(row: Row): Unit = ???
//         def waitT(): Unit = ()
//       }

//       import ImperativeObjectModel._
//       // original imperative code
//       {

//         defer.use(Use.withNoCheck) {
//           val db = Database.open
//           while (db.hasNextRow()) {
//             if (!db.lockNextRow()) doSomethingWith(db.nextRow()) else waitT()
//           }
//         }
//       }
//     }

//     {
//       object FunctionalObjectModel {
//         class Row {}
//         class Database {
//           def nextRow(): ZIO[Any, Throwable, Row] = ???
//           def hasNextRow(): ZIO[Any, Throwable, Boolean] = ???
//           def lockNextRow(): ZIO[Any, Throwable, Boolean] = ???
//         }
//         object Database {
//           def open: ZIO[Any, Throwable, Database] = ???
//         }
//         def doSomethingWith(row: Row): Unit = ???
//         def waitT(): Unit = ()
//       }
//       import FunctionalObjectModel._

//       // force user to write to vals first
//       {
//         defer {
//           val db = Database.open.run
//           while (db.hasNextRow().run) {
//             if (db.lockNextRow().run) {
//               val nextRow = db.nextRow().run
//               doSomethingWith(nextRow)
//             } else
//               waitT()
//           }
//         }
//       }
//       // that will cause correct re-write
//       {
//         Database.open.flatMap { db =>
//           def whileFun(): ZIO[Any, Throwable, Unit] =
//             db.hasNextRow().flatMap { hasNextRow =>
//               if (hasNextRow) (
//                 db.lockNextRow().flatMap { lockNextRow =>
//                   if (!lockNextRow)
//                     db.nextRow().map(nextRow => doSomethingWith(nextRow))
//                   else
//                     ZIO.succeed(waitT())
//                 }
//               ).flatMap(_ => whileFun())
//               else
//                 ZIO.unit
//             }
//           whileFun()
//         }
//       }
//     }
//   }
// }
