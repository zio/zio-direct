package zio.direct.examples

import java.io.BufferedReader
import java.io.FileReader
import zio.ZIO
import zio.direct._
import zio.direct.Dsl.Params
import zio.direct.core.metaprog.Verify

object CorrectnessExamples {


  class Boom extends Exception("Boom!")
  object Boom {
    def apply() = new Boom()
  }
  def random(): Boolean = true

  /**
   * **************************** WHILE EXAMPLES *************************************
   */
  object While {
    {
      object ImperativeObjectModel {
        class Row {}
        class Database {
          def nextRow(): Row = ???
          def hasNextRow(): Boolean = ???
          def lockNextRow(): Boolean = ???
        }
        object Database {
          def open: Database = ???
        }
        def doSomethingWith(row: Row): Unit = ???
        def waitT(): Unit = ()
      }

      import ImperativeObjectModel._
      // original imperative code
      {

        defer(Params(Verify.None)) {
          val db = Database.open
          while (db.hasNextRow()) {
            if (!db.lockNextRow()) doSomethingWith(db.nextRow()) else waitT()
          }
        }
      }
    }

    {
      object FunctionalObjectModel {
        class Row {}
        class Database {
          def nextRow(): ZIO[Any, Throwable, Row] = ???
          def hasNextRow(): Boolean = ???
          def lockNextRow(): Boolean = ???
        }
        object Database {
          def open: ZIO[Any, Throwable, Database] = ???
        }
        def doSomethingWith(row: Row): Unit = ???
        def waitT(): Unit = ()
      }
      import FunctionalObjectModel._

      // code using defer incorrectly code
      {
        defer(Params(Verify.None)) {
          val db = Database.open.run
          while (db.hasNextRow()) {
            if (db.lockNextRow()) doSomethingWith(db.nextRow().run) else waitT()
          }
        }
      }
      // wrong rewrite
      {
Database.open.flatMap { db =>
  def whileFun(): ZIO[Any, Throwable, Unit] =
    if (db.hasNextRow())
      db.nextRow().flatMap { row =>
        // Too late to check if row is locked, we already READ IT!!
        if (db.lockNextRow()) doSomethingWith(row) else waitT()
        whileFun()
      }
    else
      ZIO.unit
  whileFun()
}
      }
      // force user to write to vals first
      {
        defer(Params(Verify.None)) {
          val db = Database.open.run
          while (db.hasNextRow()) {
            if (db.lockNextRow())
              // Write it into a value first!
              val nextRow = db.nextRow().run
              doSomethingWith(nextRow)
            else
              waitT()
          }
        }
      }
      // that will cause correct re-write
      {
        Database.open.flatMap { db =>
          def whileFun(): ZIO[Any, Throwable, Unit] =
            if (db.hasNextRow())
              (
                if (!db.lockNextRow())
                  db.nextRow().map(nextRow => doSomethingWith(nextRow))
                else
                  ZIO.succeed(waitT())
              ).flatMap(_ => whileFun())
            else
              ZIO.unit
          whileFun()
        }
      }
    }
  }
}
