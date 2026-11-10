---
id: how-to-run-in-parallel
title:  "How to Run Effects in Parallel in a defer Block"
sidebar_label: "Run in Parallel"
description: "Run independent effects concurrently in a defer block with Use.withParallelEval, and see how the default sequential evaluation differs."
keywords:
  - "ZIO Direct Style"
  - "defer"
  - "parallel"
  - "Use"
  - "concurrency"
---

Use this guide when a `defer` block calls several independent effects and you want them to run at the same time instead of one after another. It explains the default evaluation order and the `Use.withParallelEval` option that changes it.

## Setup

The examples below use a `runNow` helper, as in the other guides on this site, and a `slowValue` function that waits for one second before it returns:

```scala mdoc:silent
import zio._
import zio.direct._

def runNow[A](effect: Task[A]): A =
  Unsafe.unsafe { implicit u =>
    Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
  }

def slowValue(name: String): Task[String] = ZIO.sleep(1.second).as(name)
```

## Run Effects in Sequence by Default

Without any option, each `.run` in a `defer` block waits for the previous one to finish. The next example takes two `slowValue` calls and places them in a tuple:

```scala mdoc:silent
val sequential: Task[(String, String)] =
  defer {
    (slowValue("first").run, slowValue("second").run)
  }
```

Timing the effect shows that the two one-second waits add up:

```scala mdoc
println(runNow(sequential.timed.map { case (duration, _) => duration.toMillis >= 2000L }))
```

The code above:

- `slowValue("first").run` waits for one second before the second call starts
- the tuple is built only after both values are available
- `timed` returns the elapsed time with the value, and `true` means the run took at least two seconds

## Run Effects in Parallel With Use.withParallelEval

Pass `Use.withParallelEval` to `defer` to run the effects in the same tuple at the same time. The block is otherwise unchanged:

```scala mdoc:silent
val parallel: Task[(String, String)] =
  defer(Use.withParallelEval) {
    (slowValue("first").run, slowValue("second").run)
  }
```

Timing the parallel version shows that the two waits overlap:

```scala mdoc
println(runNow(parallel.timed.map { case (duration, _) => duration.toMillis < 2000L }))
```

The parallel block returns the same tuple as the sequential block:

```scala mdoc
println(runNow(parallel))
```

:::caution
Use parallel evaluation only for effects that do not depend on each other. A `.run` call that needs the value of an earlier `.run` call cannot start before that value exists, so keep such calls in sequence.
:::

## What You've Learned

- `defer` runs `.run` calls in sequence by default
- `defer(Use.withParallelEval) { ... }` runs the `.run` calls that make up one expression concurrently
- The parallel option changes timing, not the values in the result

## Where to Go Next

- **Need the full list of supported constructs?** Read [Scala Constructs Supported by ZIO-Direct](./supported-constructs.md).
- **Want the other `Use` options?** Read [Configuration Options for defer](./configuration.md) once it is available.
