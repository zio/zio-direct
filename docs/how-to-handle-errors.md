---
id: how-to-handle-errors
title:  "How to Handle Errors in a defer Block"
sidebar_label: "Handle Errors"
description: "Catch exceptions with try and catch around .run calls in a defer block, and use unsafe to catch exceptions thrown by plain Scala code."
keywords:
  - "ZIO Direct Style"
  - "defer"
  - "error handling"
  - "try catch"
  - "unsafe"
---

Use this guide when a `defer` block calls an effect that can fail, and you want to recover from the failure with `try` and `catch`. It covers exceptions raised inside a ZIO effect, and exceptions thrown by plain Scala code that is wrapped in `unsafe`.

## Setup

The examples below use the same `runNow` helper as the tutorial, which runs an effect synchronously so that the page can print the result:

```scala mdoc:silent
import zio._
import zio.direct._

def runNow[A](effect: Task[A]): A =
  Unsafe.unsafe { implicit u =>
    Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
  }
```

## Catch an Exception From ZIO.attempt

An effect built with `ZIO.attempt` converts a thrown exception into a failure. A `try` block around `.run` catches that failure, so the `catch` clause receives the exception itself:

```scala mdoc:silent
def parseAge(text: String): Task[Int] = ZIO.attempt(text.toInt)

val withDefault: Task[Int] =
  defer {
    try parseAge("not-a-number").run
    catch {
      case _: NumberFormatException => 0
    }
  }
```

Running the effect prints the value from the `catch` clause:

```scala mdoc
println(runNow(withDefault))
```

The code above:

- `parseAge("not-a-number").run` fails with a `NumberFormatException`
- the `catch` clause matches that exception and returns `0`, which becomes the value of the `try` expression
- the `defer` block has the type `Task[Int]`, so the error channel is still tracked

## Catch a Failure From the Error Channel

An effect built with `ZIO.fail` puts a typed error into the error channel without throwing anything. Handle such an error by calling `ZIO#catchAll` on the effect, and then call `run` on the recovered effect inside the `defer` block:

```scala mdoc:silent
val failing: IO[String, Int] = ZIO.fail("disk full")

val fromFailure: Task[Int] =
  defer {
    val recovered = failing.catchAll(message => ZIO.succeed(message.length)).run
    recovered * 2
  }
```

Running the code below prints the result:

```scala mdoc
println(runNow(fromFailure))
```

The code above:

- `failing.catchAll(...)` receives the `String` error and returns an effect that succeeds with the length of the message
- `.run` gives `recovered` the value of that effect, so the failure does not reach the rest of the block
- the block returns `recovered * 2`, which is `18` for the message `disk full`

:::caution
Do not write a `catch` clause for a typed error from `ZIO.fail`. A `catch` clause in a `defer` block expects a `Throwable`, so a `String` error reaches it as a `ClassCastException` at runtime, not as the value you wrote in the pattern. Use `ZIO#catchAll` or `ZIO#catchSome` on the effect for typed errors.
:::

## Catch an Exception Thrown by Plain Scala Code

Plain Scala code that throws is not wrapped in an effect, so `defer` treats it as a pure computation. An exception thrown there is not caught by `try`. Wrap the code in `unsafe { ... }` to tell `defer` that the block can throw, and the block is then translated into `ZIO.attempt`:

```scala mdoc:silent
def divide(a: Int, b: Int): Int = a / b

val safeDivide: Task[Int] =
  defer {
    try {
      unsafe {
        divide(10, 0)
      }
    } catch {
      case _: ArithmeticException => -1
    }
  }
```

Running the code below prints the result:

```scala mdoc
println(runNow(safeDivide))
```

The code above:

- `divide(10, 0)` throws an `ArithmeticException` at runtime
- the `unsafe` block makes `defer` wrap the call in `ZIO.attempt`, so the exception becomes a failure
- the `catch` clause returns `-1`, which is the value of the whole block

## What You've Learned

- `try` and `catch` around `.run` catch the exceptions of effects built with `ZIO.attempt`
- Typed errors from `ZIO.fail` are handled with `ZIO#catchAll` on the effect
- `unsafe { ... }` lets `try` catch exceptions thrown by plain Scala code

## Where to Go Next

- **Need the full list of constructs?** Read [Scala Constructs Supported by ZIO-Direct](./supported-constructs.md).
