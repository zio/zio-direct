package zio.direct.examples

import java.io.BufferedReader
import java.io.FileReader
import zio.ZIO
import zio.direct._
import zio.direct.Dsl.Params
import zio.direct.core.metaprog.Verify
import java.io.File

object IntroductionExamples {

  /**
   * **************************** Sequence *************************************
   */
  object Sequence {
    object ImperativeObjectModel {
      def read(file: File): String = ???
      def write(file: File, content: String): Unit = ???
      def fileA: File = ???
      def fileB: File = ???
      def fileC: File = ???
    }
    {
      import ImperativeObjectModel._
      val textA = read(fileA)
      val textB = read(fileB)
      write(fileC, textA + textB)
    }

    object FunctionalObjectModel {
      def read(file: File): ZIO[Any, Throwable, String] = ???
      def write(file: File, content: String): ZIO[Any, Throwable, Unit] = ???
      def fileA: File = ???
      def fileB: File = ???
      def fileC: File = ???

      // regular way
      {
        read(fileA).flatMap { textA =>
          read(fileB).flatMap { textB =>
            write(fileC, textA + textB)
          }
        }
      }
      // for-comprehension way
      {
        for {
          textA <- read(fileA)
          textB <- read(fileB)
          _ <- write(fileC, textA + textB)
        } yield ()
      }
      // using deferA
      {
        defer {
          val textA = run(read(fileA))
          val textB = run(read(fileB))
          run(write(fileC, textA + textB))
        }
      }
      // defer: Op => ZIO[E, R, Op]
      // run: ZIO[E, R, Op] => Op
      // explain how .run is same as run(...)
      // using deferB
      {
        defer {
          val textA = read(fileA).run
          val textB = read(fileB).run
          write(fileC, textA + textB).run
        }
      }
    }
  }

  /**
   * **************************** IF *************************************
   */
  object If {
    {
      object ImperativeObjectModel {
        class Row {}
        class Database {
          def transactionsEnabled(): Boolean = ???
          def lazyFetchEnabled(): Boolean = ???
          def bulkInsert(row: List[Row]): Unit = ???
        }
        object Database {
          def open(): Database = ???
        }
        def waitT(): Unit = ()
      }
      import ImperativeObjectModel._

      // original imperetive code
      {
        val rows: List[Row] = ???
        val db = Database.open()
        if (db.transactionsEnabled() && db.lazyFetchEnabled()) {
          db.bulkInsert(rows)
        }
      }
    }
    {
      object FunctionalObjectModel {
        class Row {}
        class Database {
          def transactionsEnabled(): ZIO[Any, Throwable, Boolean] = ???
          def lazyFetchEnabled(): ZIO[Any, Throwable, Boolean] = ???
          def bulkInsert(row: List[Row]): ZIO[Any, Throwable, Unit] = ???
        }
        object Database {
          def open(): ZIO[Any, Throwable, Database] = ???
        }
        def doSomethingWith(row: Row): Unit = ???
        def waitT(): Unit = ()
      }

      import FunctionalObjectModel._
      // Functional code
      {
        val rows: List[Row] = ???
        Database.open().flatMap { db =>
          db.transactionsEnabled().flatMap { te =>
            if (te)
              db.lazyFetchEnabled().flatMap { lf =>
                if (lf)
                  db.bulkInsert(rows)
                else
                  ZIO.unit
              }
            else
              ZIO.unit
          }
        }
      }
      // Defer code
      {
        val rows: List[Row] = ???
        defer {
          val db = Database.open().run
          if (db.transactionsEnabled().run && db.lazyFetchEnabled().run) {
            db.bulkInsert(rows).run
          }
        }
      }
    }
  }

  /**
   * **************************** WHILE *************************************
   */
  object While {

    def imperativeSimpleExample(): Unit = {
      val path = "src/test/resources/file_example.txt"
      def makeFile() = new BufferedReader(new FileReader(path))
      val file = makeFile()
      val buffer = new StringBuffer()

      var line: String = file.readLine()
      while (line != null) {
        buffer.append(line)
        line = file.readLine()
      }

      file.close()
    }
    def functionalSimpleExample(): Unit = {
      val path = "src/test/resources/file_example.txt"
      def makeFile() = new BufferedReader(new FileReader(path))
      ZIO.attempt(makeFile()).flatMap { file =>
        val buffer = new StringBuffer()

        ZIO.attempt(file.readLine()).flatMap { line0 =>
          var line = line0
          def whileFun(): ZIO[Any, Throwable, Unit] =
            if (line != null)
              buffer.append(line)
              ZIO.attempt(file.readLine()).flatMap { lineN =>
                line = lineN
                whileFun()
              }
            else
              ZIO.unit
          whileFun()
        }
      }

    }
  }
}
