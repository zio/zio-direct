---
id: getting-started
title:  "Getting Started: Your First defer Block"
sidebar_label: "Getting Started"
description: "Learn ZIO Direct Style by rewriting a ZIO for-comprehension as a defer block, then adding a conditional that for-comprehensions cannot express."
keywords:
  - "ZIO Direct Style"
  - "defer"
  - "ZIO"
  - "Direct Style"
---

Welcome to ZIO Direct Style! This tutorial is for Scala developers who know basic ZIO effects such as `ZIO.succeed` and `ZIO.attempt`, and who have written a for-comprehension before. You don't need any prior experience with ZIO Direct Style to follow along.

## Learning Objectives

By the end of this tutorial, you will understand:

- Why a for-comprehension turns sequential effects into nested `flatMap` calls
- How a `defer { ... }` block and the `run` suffix replace those `flatMap` calls
- Why an `if` that depends on an effect fits in a `defer` block but not in a for-comprehension

We'll learn these ideas through three steps: a for-comprehension, the same program written with `defer`, and a conditional.

We recommend reading from top to bottom. Each step reuses the setup from the step before.

## Background

A ZIO effect such as `Task[String]` describes a computation without running it. To use the value an effect produces, you call `flatMap`, and each additional effect in a row adds another level of nesting. A for-comprehension hides that nesting, but it only supports sequencing: each line binds one value, and you cannot place an `if` between two binds.

ZIO Direct Style lets you write the same program as an ordinary block. Inside a `defer` block, each `run` marks the point where the value of an effect is needed. The macro rewrites the block into the `flatMap` chain for you at compile time.

## 1. Start With a For-Comprehension

We'll model a small job: load a user, load that user's theme, then build a greeting. Each function returns a ZIO effect, and no effect produces a value until it runs. The helper `runNow` runs an effect synchronously so that this page can print the result. Its definition, together with the three functions, looks like this:

```scala mdoc:silent
import zio._
import zio.direct._

def loadUser(id: Int): Task[String] = ZIO.attempt(s"user-$id")
def loadTheme(user: String): Task[String] = ZIO.succeed(s"dark-theme-for-$user")
def greet(user: String, theme: String): Task[String] = ZIO.succeed(s"Hello $user ($theme)")

def runNow[A](effect: Task[A]): A =
  Unsafe.unsafe { implicit u =>
    Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
  }
```

The for-comprehension below combines the three functions:

```scala mdoc:silent
val forResult: Task[String] =
  for {
    user  <- loadUser(42)
    theme <- loadTheme(user)
    text  <- greet(user, theme)
  } yield text
```

Running the effect prints the greeting:

```scala mdoc
println(runNow(forResult))
```

The code above:

- `user <- loadUser(42)` runs `loadUser` and names its result `user`
- `theme <- loadTheme(user)` uses `user` from the line before it
- `text <- greet(user, theme)` builds the greeting from both values
- `yield text` produces the final effect, which has the type `Task[String]`

## 2. Write the Same Program With defer

The same program can be written with a `defer` block. Each `<-` becomes a `val` with a `run` suffix on the effect:

```scala mdoc:silent
val deferResult: Task[String] =
  defer {
    val user = loadUser(42).run
    val theme = loadTheme(user).run
    greet(user, theme).run
  }
```

Running it gives the same output as the for-comprehension:

```scala mdoc
println(runNow(deferResult))
```

The code above:

- `loadUser(42).run` waits for the effect and gives its value to `user`
- `loadTheme(user).run` uses `user` exactly as a normal `val` would
- `greet(user, theme).run` is the last expression, so its String is the result of the block
- `defer { ... }` has the type `Task[String]`, the same as the for-comprehension

:::note
The `run` suffix is only valid inside a `defer` block. Outside of one, the call compiles but throws `NotDeferredException` when it runs.
:::

## 3. Add a Conditional

A for-comprehension cannot place an `if` between two binds, because the `if` would need to decide what happens next based on a value that is not yet available. In a `defer` block, the `if` is ordinary Scala, and effects can appear in both branches:

```scala mdoc:silent
val conditional: Task[String] =
  defer {
    val user = loadUser(42).run
    if (user.endsWith("42")) {
      val theme = loadTheme(user).run
      greet(user, theme).run
    } else
      "Guest"
  }
```

Running it shows the first branch, because the user name ends in `42`:

```scala mdoc
println(runNow(conditional))
```

The `loadTheme` effect runs only when the condition is true. An `if` in a `defer` block skips the effects in the branch that is not taken, the same as a normal `if` skips its branches.

:::tip
Treat `run` as the only new syntax. Everything else in a `defer` block is plain Scala, so any function that returns a ZIO effect works with the `run` suffix and keeps its signature.
:::

## What You've Learned

In this tutorial, you learned:

- Why a for-comprehension nests `flatMap` calls, and what it cannot express
- How `defer { ... }` and `run` replace those `flatMap` calls
- How an `if` with effects inside its branches works in a `defer` block

You now have the basic idea of ZIO Direct Style. The next step is to see which other Scala constructs work inside a `defer` block.

## Where to Go Next

- **Want the full list of supported constructs?** Read the reference page [Scala Constructs Supported by ZIO-Direct](./supported-constructs.md), which covers `match`, `try`, `while`, and more.
- **Want to use ZIO Direct Style with streams or pure computations?** Read [Other Supported Monads](./other-supported-monads.md).
- **Using IntelliJ?** Read [IntelliJ Support](./intellij-support.md) before writing `defer` blocks.
