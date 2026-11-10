---
id: other-supported-monads
title:  "Other Supported Monads"
sidebar_label: "Other Supported Monads"
description: "How to use zio-direct with ZStream, ZPure, scala.concurrent.Future, and scala.List, with the import and evaluation method each one needs."
keywords:
  - "ZIO Direct Style"
  - "ZStream"
  - "ZPure"
  - "Future"
  - "List"
---

Besides ZIO, zio-direct supports ZStream, ZPure, `scala.concurrent.Future`, and `scala.List`. Each one has its own `defer` and its own evaluation method, so you import the one you need. The ZStream and ZPure modules are separate artifacts, and the Future and List support is included in the core library.

> Note that all of these are currently only supported in Scala 3.

## ZStream

To use zio-direct with ZStream, add the following to your `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-direct-streams" % "@VERSION@"
```

Import `zio.direct.stream._`, and call `each` on each stream to get its elements. Inside a stream `defer` block, `each` plays the role that `run` plays in a ZIO block:

```scala mdoc:silent:reset
import zio._
import zio.stream._
import zio.direct.stream._

val out =
  defer {
    val a = ZStream(1, 2, 3).each
    val b = ZStream("foo", "bar").each
    (a, b)
  }
```

Running the stream collects every combination of the elements:

```scala mdoc
println(Unsafe.unsafe { implicit u => Runtime.default.unsafe.run(out.runCollect).getOrThrowFiberFailure() })
```

If you also use zio-direct with ZIO, rename the stream `defer` to avoid a name clash. The stream `defer` is renamed to `deferStream`, and the ZIO `defer` is imported from `zio.direct`:

```scala mdoc:silent:reset
import zio._
import zio.stream._
import zio.direct.stream.{ defer => deferStream, each }
import zio.direct._

val outStream: ZStream[Any, Nothing, (Int, String)] =
  deferStream {
    val a = ZStream(1, 2, 3).each
    val b = ZStream("foo", "bar").each
    (a, b)
  }

val outZio: ZIO[Any, Nothing, Chunk[(Int, String)]] =
  defer {
    val a: Chunk[(Int, String)] = outStream.runCollect.run
    val b = ZIO.succeed((123, "baz")).run
    a :+ b
  }
```

Running the code below prints the result:

```scala mdoc
println(Unsafe.unsafe { implicit u => Runtime.default.unsafe.run(outZio).getOrThrowFiberFailure() })
```

## ZPure

To use zio-direct with ZPure, add the following to your `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-direct-pure" % "@VERSION@"
```

Before writing a `defer` block, define a context with `deferWith[W, S]`, where `W` is the log type and `S` is the state type. Import the members of that context, and use `eval` to get the value of a ZPure computation:

```scala mdoc:silent:reset
import zio.direct.pure._
import zio.prelude.fx.ZPure

case class MyState(value: String)

val dc = deferWith[String, MyState]
import dc._

val out =
  defer {
    val s1 = ZPure.get[MyState].eval.value
    ZPure.set(MyState("bar")).eval
    val s2 = ZPure.get[MyState].eval.value
    (s1, s2)
  }
```

Providing the initial state and running the computation gives the two states in order:

```scala mdoc
println(out.provideState(MyState("foo")).run)
```

The helpers `getState()`, `setState(...)`, and `log(...)` are members of the context, so they are available after `import dc._`. The same computation with the helpers:

```scala mdoc:silent
val outHelpers =
  defer {
    val s1 = getState().value
    setState(MyState("bar"))
    val s2 = getState().value
    (s1, s2)
  }
```

Running the code below prints the result:

```scala mdoc
println(outHelpers.provideState(MyState("foo")).run)
```

`provideState` is a method of the ZPure type from zio-prelude, not a zio-direct method. It supplies the initial state before the computation runs.

:::note
Metals auto-complete and type popups can be slow with ZPure, especially when `try` and `catch` are used. The compile itself is quick.
:::

## List

Support for Scala's `List` is provided by `zio.direct.list`. The entry point is `select` rather than `defer`, and each element of a list is taken with `from`:

```scala mdoc:reset
import zio.direct.list._

val pairs =
  select {
    val a = from(List(1, 2, 3))
    val b = from(List("foo", "bar"))
    (a, b)
  }

println(pairs)
```

The result contains every combination of the elements, in the same order as the equivalent for-comprehension.

## Future

Support for `scala.concurrent.Future` is provided by `zio.direct.future`. The `defer` block takes an implicit `ExecutionContext` in scope, and each future is taken with `.run`:

```scala mdoc:reset
import zio.direct.future._
import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

val future =
  defer {
    Future("a").run match {
      case "a" => Future(1).run
      case _   => Future(2).run
    }
  }

println(Await.result(future, 1.second))
```

Any `ExecutionContext` works, not only the global one. The context only needs to be in scope when `defer` is called:

```scala mdoc
def withContext(implicit ec: ExecutionContext): Future[Int] =
  defer {
    Future("a").run match {
      case "a" => Future(1).run
      case _   => Future(2).run
    }
  }

println(Await.result(withContext(ExecutionContext.global), 1.second))
```
