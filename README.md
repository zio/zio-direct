# ZIO direct

| Project Stage | CI | Release | Snapshot | Discord |
| --- | --- | --- | --- | --- |
| [![Project stage][Badge-Stage]][Link-Stage-Page] | ![CI][Badge-CI] | [![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases] | [![Snapshot Artifacts][Badge-SonatypeSnapshots]][Link-SonatypeSnapshots] | [![Badge-Discord]][Link-Discord] |

# Summary
Direct-Style programming in ZIO based on the Monadless paradigm.


# Documentation

Talk at Functional Scala 2022
https://www.slideshare.net/deusaquilus/ziodirect-functional-scala-2022

To use zio-direct, add the following to your `build.sbt` file.

```scala
libraryDependencies += "dev.zio" % "zio-direct_3" % "1.0.0-RC1"
```

ZIO-Direct allows direct style programming with ZIO. This library provides a *syntactic sugar* that is more powerful than for-comprehensions as well as more natural to use. Simply add the `.run` suffix to any ZIO effect in order to retrieve it's value.

ZIO-Direct works by using macros to rewrite sequential code into flatMap-chains based on the [Monadless](https://github.com/monadless/monadless) paradigm. The values resulting in `.run` calls from the ZIO effects are not actually awaited. Instead, they are rolled-up into a chain of flatMaps.

For example, in imperative programming operations typically are done in a simple set of steps.
```scala
object FileOps:
  def read(file: File): String
  def write(file: File, content: String): Unit

val textA = read(fileA)
val textB = read(fileB)
write(fileC, textA + textB)
```

Using functional programming, the equivalent of this functionality is a set of nested flatMap-chains.
```scala
object FileOps
  def read(file: File): ZIO[Any, Throwable, String]
  def write(file: File, content: String): ZIO[Any, Throwable, Unit]

read(fileA).flatMap { textA =>
  read(fileB).flatMap { textB =>
    write(fileC, textA + textB)
  }
}
```

In order to avoid this complexity scala provides a for-comprehension syntactic sugar.
```scala
for {
  textA <- read(fileA)
  textB <- read(fileB)
  _ <- write(fileC, textA + textB)
} yield ()
```

Unfortunately this syntactic sugar is limited in many cases, for example, inserting a conditional value inside is impossible.
```scala
for {
  textA <- read(fileA)
  // Not a possible syntax
  if (fileA.contains("some string")) {
    textB <- read(fileB)
    _ <- write(fileC, textA + textB)
  }
} yield ()
```

ZIO-Direct offers an equivalent syntactic sugar that is more ergonomic and allows many constructs that for-comprehensions do not.
```scala
defer {
  val textA = read(fileA).run
  if (fileA.contains("some string")) {
    val textB = read(fileB).run
    write(fileC, textA + textB).run
  }
}
```


# ZIO-Tailored
ZIO-Direct is specifically tailored to ZIO capabilities as it supports Environment and Error composition in ZIO effects similar to the for-comprehension.

```scala
val out: ZIO[CustomerConfig & DistributorConfig, CustomerGetException | DistrubutorGetException, (Customer, Distributor)] =
  defer {
    // Get a customer-configuration object from the environment and extract its .url field
    val custUrl: String = ZIO.service[CustomerConfig].run.url
    // Get a distributor-configuration from the environment and extract its .url field
    val distUrl: String = ZIO.service[DistributorConfig].run.url
    (
      // Use the two configurations to make an HTTP-call
      parseCustomer(httpGetCustomer(custUrl).run),
      parseDistrubutor(httpGetDistributor(distUrl).run)
    )
  }
```

## Branching and Looping Support
Unlike the for-comprehension, ZIO-Direct supports branching and looping in the use of flatMaps composition.
Let's have a look at a another non-trivial example.

```scala
class Database:
  def nextRow(): ZIO[Any, Throwable, Row]
  def hasNextRow(): ZIO[Any, Throwable, Boolean]
  def lockNextRow(): ZIO[Any, Throwable, Boolean]
object Database:
  def open: ZIO[Any, Throwable, Database]

defer {
  // Open a database connection
  val db = Database.open().run
  // See if there is is a next-row
  while (db.hasNextRow().run) {
    // try to lock, if aquired continue
    if (db.lockNextRow().run)
      val nextRow = db.nextRow().run
      doSomethingWith(nextRow)
    else
      waitT()
  }
}
```
> NOTE: The above database-api is imaginary.

The above code needs to be translated into something like this:

```scala
Database.open.flatMap { db =>
  def whileFun(): ZIO[Any, Throwable, Unit] =
    db.hasNextRow().flatMap { hasNextRow =>
      if (hasNextRow)(
        db.lockNextRow().flatMap { lockNextRow =>
          if (!lockNextRow)
            db.nextRow().map(nextRow => doSomethingWith(nextRow))
          else
            ZIO.succeed(waitT())
        }
      ).flatMap(_ => whileFun())
      else
        ZIO.unit
    }
  whileFun()
}
```

Note that normally this is the exact code that would have to be written to capture such functionality For-comprehensions do not provide a way to do looping and branching so in such cases
a combination of flatMaps and recursion is necessary to avoid calling effects unnecessarily.

## Great for Refs and FiberRefs!

ZIO-direct makes it much easier to use ZIO mutable Ref and FiberRef variables. Since retrieveing and updating Ref and FiberRef variables requries a flatMap/for-comprehension call, it is typically very difficult to use them with branching/looping constructs. ZIO-direct makes these cases much easier.

```scala
class DatabaseApi {
  val connRef = FiberRef.make[Option[Connection]](None)
  def openConnection(): Connection = lowLevelDatabaseApi.openConnection()
  def transaction(action: Action) =
    defer {
      val conn = connRef.get.run
      if (conn == None) {
        connRef.set(Some(openConnection())).run
      }
      val conn1 = connRef.get.run
      conn.execute(action).run
    }
}
```

Instead of having to write the following code:
```scala
class DatabaseApi {
  val connRef = FiberRef.make[Option[Connection]](None)
  def openConnection(): Connection = lowLevelDatabaseApi.openConnection()
  def transaction(action: Action) =
    connRef.get.flatMap { conn =>
      (if (conn == None) {
        connRef.set(Some(openConnection()))
      } else {
        ZIO.unit
      }).flatMap(_ => connRef.get.flatMap { conn1 =>
        conn1.execute(action)
      })
    }

  // Note that for-comprehensions do not help very much in this use-case
  def transaction0(action: Action) =
    for {
      conn <- connRef.get
      _ <- if (conn == None) {
        connRef.set(Some(openConnection()))
      } else {
        ZIO.unit
      }
      conn1 <- connRef.get
      _ <- conn1.execute(action)
    }
}
```


# Supported Constructs

ZIO-direct supports using the following constructs inside of a `defer` block. Approximate translations of the what the Scala code looks like are available below. In order to see the exact translations for any code in a defer block, use `defer.info`.

### blocks

```scala
defer {
  val a = ZIO.succeed("Hello").run
  val b = ZIO.succeed("World").run
  a + " " + b
}
```
Translation:
```scala
ZIO.succeed("Hello").flatMap { a =>
  ZIO.succeed("World").map { b =>
    a + " " + b
  }
}
```

Blocks can also have nested blocks.
```scala
defer {
  val a = ZIO.succeed("Hello").run
  val b = {
    val x = ZIO.succeed("to").run
    val y = ZIO.succeed("World").run
    x + " " + y
  }
  a + " " + b
}
```
Translation:
```scala
ZIO.succeed("Hello").flatMap { a =>
  {
    ZIO.succeed("to").flatMap { x =>
      ZIO.succeed("World").map { y =>
        x + " " + y
      }
    }
  }.map { b =>
    a + " " + b
  }
}
```




### if/else
If statements with one or multiple ZIO.run values in the condition(s) and action(s).

```scala
defer {
  if (ZIO.succeed(123).run < 456 && ZIO.succeed("foo") == "foo")
    ZIO.succeed("a").run
  else
    ZIO.succeed("b").run
}
```

Translation:
> Note that each condition is separated into it's own nested flatMap chain step
(from left-to-right) so if earlier conditions yield `false` ZIO computations of
later ones will not be executed.
```scala
ZIO.succeed(123).flatMap { a =>
  if (a < 456)
    ZIO.succeed("foo").flatMap { b =>
      if (b == "foo")
        ZIO.succeed("a")
      else
        ZIO.succeed("b")
    }
  else
    ZIO.succeed("b")
}
```


### match

Match statements with ZIO.run in the left-hand-side (before "match") and/or the right-hand-side (after the "=>").
ZIO.run calls inside of match guards (i.e. if-statements after `case Clause`) are not supported yet.

```scala
defer {
  ZIO.succeed("Hello").run match {
    case hello @ "Hello" =>
      val world = ZIO.succeed(" World").run
      hello + " " + world
    case _ =>
      "Nothing"
  }
}
```

Translation:
```scala
ZIO.succeed("Hello").flatMap { x =>
  x match {
    case hello @ "Hello" =>
      ZIO.succeed(" World").flatMap { world =>
        hello + " " + world
      }
    case _ =>
      ZIO.succeed("Nothing")
  }
}
```

### try

Try statements with ZIO.run in the left-hand-side (before "try") and/or the right-hand-side (after the "=>").

```scala
defer {
  try {
    val a = ZIO.succeed(123).run
    val b = ZIO.attempt(somethingUnsafe).run
    a + b
  } catch {
    case e: Exception =>
      ZIO.succeed(789).run
  }
}
```

Translation:
```scala
ZIO.succeed(123).flatMap { a =>
  ZIO.attempt(somethingUnsafe).map { b =>
    a + b
  }.catchAll { e =>
    ZIO.succeed(789)
  }
}
```
Note that because try-statements are translated into ZIO.catchAll, errors that go into fail fail-channel
will not be caught by the catch block. For example:

```scala
def throwsException() = throw new Exception("foo")

defer {
  try {
    // Will not be caught!!
    ZIO.succeed(throwsException()).run
  } catch {
    case e: Exception => 123
  }
}

defer {
  try {
    // WILL be caught!!
    ZIO.attemt(throwsException()).run
  } catch {
    case e: Exception => 123
  }
}
```

In cases where methods that throw exceptions not not wrapped into ZIO computations, they will also
not be caught because the assumption is that they are pure-computations hence can be wrapped into `ZIO.succeed` blocks.

```scala
def throwsException() = throw new Exception("foo")

defer {
  try {
    // Will not be caught!!
    throwsException()
  } catch {
    case e: Exception => 123
  }
}

// Translation:
ZIO.succeed(throwsException()).catchAll { e =>
  case e: Exception => 123
}
```

In order to rectify this situation, a region-based operator `unsafe { ... }` can be used to wrap
all blocks of code that could potentially throw exceptions. ZIO-Direct will the know to wrap
them into `ZIO.attempt` clauses instead of `ZIO.succeed`.
```scala
def throwsException() = throw new Exception("foo")

defer {
  try {
    unsafe {
      // This WILL be caught!!
      throwsException()
    }
  } catch {
    case e: Exception => 123
  }
}

// Translation:
ZIO.attempt(throwsException()).catchAll { e =>
  case e: Exception => 123
}
```

Note that that ZIO computations with `.run` calls and other kinds of constructs supported by zio-direct
can be used inside of `unsafe` blocks, and these computations will be used as-is (i.e. if they contain
ZIO.succeed calls the will not be changed into something else).

```scala
defer {
  try {
    unsafe {
      val a = ZIO.succeed(123).run
      throwsException()
      val b = ZIO.succeed(456).run
      a + b
    }
  } catch {
    case e: Exception => 123
  }
}

// Translation:
ZIO.succeed(123).flatMap { a =>
  ZIO.attempt(throwsException()).flatMap { _ =>
    ZIO.succeed(456).map { b =>
      a + b
    }
  }
}.catchAll { e =>
  case e: Exception => 123
}
```

### while

While-clauses will be translated into recursive functions that conditionally recurse into a flatMap call based on the while-condition.

> Generally due to the presence of functions like [`ZIO.iterate`](https://zio.dev/reference/control-flow/#iterate) and [`ZIO.repeat`](https://zio.dev/reference/test/aspects/repeat-and-retry/#repeat) the critical use-case for ZIO-direct's while-loop should be limited.

```scala
// Note that because mutable variable usage is generally not allowed in zio-direct the below code can only be run in "Lenient Mode."
var i = 0
defer {
  while (i < 10) {
    ZIO.attempt(println("Hello")).run
    i += 1
  }
}

// Translation:
val i = 0
def loop(): ZIO[Any, Throwable, Unit] = {
  if (i < 10) {
    ZIO.attempt(println("Hello")).flatMap { _ =>
      i += 1
      loop()
    }
  } else {
    ZIO.unit
  }
}
loop()
```


Since mutable variables are generally not allowed in `defer { ... }` blocks, it is recommended to use mutable references from ZIO's
[`Ref`](https://zio.dev/reference/concurrency/ref/) class instead.

```scala
defer {
  val ref = Ref.make(0).run
  while (ref.get.run < 10) {
    ZIO.attempt(println("Hello")).run
    ref.update(_ + 1).run
  }
}

// Translation:
Ref.make(0).flatMap { ref =>
  def loop(): ZIO[Any, Throwable, Unit] = {
    ref.get.flatMap { x =>
      if (x < 10) {
        ZIO.attempt(println("Hello")).flatMap { _ =>
          ref.update(_ + 1).flatMap { _ =>
            loop()
          }
        }
      } else {
        ZIO.unit
      }
    }
  }
  loop()
}
```

### for-loop/foreach

Scala for-loops and collection.foreach are the same thing (the former dis desugars into the latter).
ZIO-direct will translate them into ZIO.foreach calls.

> Similar to while-loops, this construct is
largely overshawoed by ZIO's own [`foreach`](https://zio.dev/reference/control-flow/#iterate) and [`iterate`](https://zio.dev/reference/control-flow/#foreach) combinators.

```scala
defer {
  for (i <- 1 to 10) {
    ZIO.attempt(println(i)).run
  }
}

// Translation:
ZIO.foreach(1 to 10) { i =>
  ZIO.attempt(println(i))
}.map(_ => ()) // since the final result must have a type of Unit
```


## Code of Conduct

See the [Code of Conduct](https://zio.github.io/zio-direct/docs/about/about_coc)

## Support

Come chat with us on [![Badge-Discord]][Link-Discord].


# License
[License](LICENSE)

[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.zio/zio-direct_2.12.svg "Sonatype Releases"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/dev.zio/zio-direct_2.12.svg "Sonatype Snapshots"
[Badge-Discord]: https://img.shields.io/discord/629491597070827530?logo=discord "chat on discord"
[Badge-CI]: https://github.com/zio/zio-direct/workflows/CI/badge.svg
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/dev/zio/zio-direct_2.12/ "Sonatype Releases"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/dev/zio/zio-direct_2.12/ "Sonatype Snapshots"
[Link-Discord]: https://discord.gg/2ccFBr4 "Discord"
[Badge-Stage]: https://img.shields.io/badge/Project%20Stage-Development-yellowgreen.svg
[Link-Stage-Page]: https://github.com/zio/zio/wiki/Project-Stages
