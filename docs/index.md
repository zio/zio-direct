---
id: index
title:  "Introduction to ZIO Direct Style"
sidebar_label: "Introduction"
---

ZIO Direct Style is a library that allows using directly-style i.e. imperative programming with ZIO effects which is based on the Monadless paradigm.

@PROJECT_BADGES@

## Installation

To use zio-direct, add the following to your `build.sbt` file.

```scala
libraryDependencies += "dev.zio" %% "zio-direct" % "@VERSION@"
```

You can also use zio-direct with ZStream and ZPure by importing the following modules.
> Currently this is only supported in Scala 3.

```scala
// ZStream
libraryDependencies += "dev.zio" %% "zio-direct-streams" % "@VERSION@"
// ZPure
libraryDependencies += "dev.zio" %% "zio-direct-pure" % "@VERSION@"
```
See the [Other Supported Monads](other-supported-monads.md) section for more details.

## IDE Support

The preferred IDE to use with ZIO-Direct is Visual Studio Code + Metals. This is because Metals correctly reads the returns from `defer` calls directly from the Scala compiler which is not the case of IntelliJ. To remedy this issue, a Library Extension is provided for ZIO-Direct. See the [IntelliJ Support](intellij-support.md) section for more details.

## Introduction

Talk at Functional Scala 2022:
* Video - https://www.youtube.com/watch?v=DgqfLfzq5eo
* Slides - https://www.slideshare.net/deusaquilus/ziodirect-functional-scala-2022

ZIO-Direct allows direct style programming with ZIO. This library provides a *syntactic sugar* that is more powerful than for-comprehensions as well as more natural to use. Simply add the `.run` suffix to any ZIO effect in order to retrieve its value.

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


## ZIO-Tailored

ZIO-Direct is specifically tailored to ZIO capabilities. It composes the environment and error types of ZIO effects, in the same way as a for-comprehension does. The setup block defines the services and functions used below:

```scala mdoc:silent:reset
import zio._
import zio.direct._

case class CustomerConfig(url: String)
case class DistributorConfig(url: String)

def httpGetCustomer(url: String): Task[String] = ZIO.succeed(s"customer at $url")
def httpGetDistributor(url: String): Task[String] = ZIO.succeed(s"distributor at $url")
def parseCustomer(text: String): Task[String] = ZIO.succeed(text.toUpperCase)
def parseDistributor(text: String): Task[String] = ZIO.succeed(text.toUpperCase)
```

The `defer` block reads both configurations from the environment, and its type is inferred from the effects it runs:

```scala mdoc:silent
val out: ZIO[CustomerConfig & DistributorConfig, Throwable, (String, String)] =
  defer {
    // Get a customer-configuration object from the environment and extract its .url field
    val custUrl: String = ZIO.service[CustomerConfig].run.url
    // Get a distributor-configuration from the environment and extract its .url field
    val distUrl: String = ZIO.service[DistributorConfig].run.url
    // Use the two configurations to make HTTP-calls
    val custText = httpGetCustomer(custUrl).run
    val distText = httpGetDistributor(distUrl).run
    // Parse each response
    (parseCustomer(custText).run, parseDistributor(distText).run)
  }
```

Providing the two configurations and running the effect prints the parsed results:

```scala mdoc
println(Unsafe.unsafe { implicit u =>
  Runtime.default.unsafe
    .run(out.provide(ZLayer.succeed(CustomerConfig("cust.example")), ZLayer.succeed(DistributorConfig("dist.example"))))
    .getOrThrowFiberFailure()
})
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

## Great for Refs and FiberRefs

ZIO-direct makes it much easier to use ZIO `Ref` and `FiberRef` variables. Retrieving and updating a `Ref` requires a `flatMap` or for-comprehension call, so branching around it quickly becomes hard to read. In a `defer` block, the `.run` suffix retrieves and updates the reference in place:

```scala mdoc:silent
val refDemo: Task[Int] =
  defer {
    val counter = Ref.make(0).run
    if (counter.get.run == 0) counter.set(5).run
    counter.get.run
  }

```

```scala mdoc
println(Unsafe.unsafe { implicit u => Runtime.default.unsafe.run(refDemo).getOrThrowFiberFailure() })
```

The same logic without `defer` nests the `get` and `set` calls inside `flatMap` and `map`, which is the code the macro writes for you.

## Next Steps

- **New to ZIO Direct Style?** Follow the [tutorial](./tutorial-rewrite-for-comprehension.md), which rewrites a for-comprehension as a `defer` block.
- **Handling failures?** Read [How to Handle Errors in a defer Block](./how-to-handle-errors.md).
- **Running effects in parallel?** Read [How to Run Effects in Parallel in a defer Block](./how-to-run-in-parallel.md).
- **Looking for a specific construct or message?** Read [Scala Constructs Supported by ZIO-Direct](./supported-constructs.md) or [Restrictions and Error Messages](./restrictions-and-error-messages.md).
