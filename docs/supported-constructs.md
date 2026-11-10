---
id: supported-constructs
title:  "Scala Constructs Supported by ZIO-Direct"
sidebar_label: "Supported Constructs"
description: "Reference for the Scala constructs that work inside a defer block, with a compiled example for each, the translation zio-direct generates, and the limits of try, while, and match."
keywords:
  - "ZIO Direct Style"
  - "defer"
  - "Scala constructs"
  - "try catch"
  - "while loop"
---

ZIO-direct supports using the following constructs inside of a `defer` block. Each example below compiles with the docs build, and each translation shows the approximate code that zio-direct generates. To see the exact translation of any block, use `defer.info`, as described in the [debugging guide](./how-to-debug-defer.md).

The examples below share this setup, which defines the helper that runs each effect:

```scala mdoc:silent
import zio._
import zio.direct._

def throwsException(): Int = throw new Exception("foo")
def somethingUnsafe: Int = 42

def runNow[A](effect: Task[A]): A =
  Unsafe.unsafe { implicit u =>
    Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
  }
```

## Blocks

A `defer` block can contain several `val` definitions. Each `.run` call gives a `val` the value of an effect, and the last expression is the value of the block:

```scala mdoc
println(runNow(defer {
  val a = ZIO.succeed("Hello").run
  val b = ZIO.succeed("World").run
  a + " " + b
}))
```

Translation:

```scala
ZIO.succeed("Hello").flatMap { a =>
  ZIO.succeed("World").map { b =>
    a + " " + b
  }
}
```

A block can also contain a nested `defer` block, whose result is then used like any other value:

```scala mdoc
println(runNow(defer {
  val a = ZIO.succeed("Hello").run
  val b = defer {
    val x = ZIO.succeed("to").run
    val y = ZIO.succeed("World").run
    x + " " + y
  }.run
  a + " " + b
}))
```

## if/else

An `if` statement can contain `.run` calls in its condition and in its branches:

```scala mdoc
println(runNow(defer {
  if (ZIO.succeed(123).run < 456 && ZIO.succeed("foo").run == "foo")
    ZIO.succeed("a").run
  else
    ZIO.succeed("b").run
}))
```

Translation:

```scala
ZIO.succeed(123).flatMap { a =>
  if (a < 456)
    ZIO.succeed("foo").flatMap { b =>
      if (b == "foo") ZIO.succeed("a") else ZIO.succeed("b")
    }
  else
    ZIO.succeed("b")
}
```

Each condition becomes its own step, from left to right. If an earlier condition is `false`, the effects in the later conditions do not run.

## match

A `match` statement can contain `.run` calls before the `match` keyword and inside the case bodies:

```scala mdoc
println(runNow(defer {
  ZIO.succeed("Hello").run match {
    case hello @ "Hello" =>
      val world = ZIO.succeed(" World").run
      hello + world
    case _ =>
      "Nothing"
  }
}))
```

:::caution
`.run` calls inside a guard, which is the `if` after a `case` pattern, are not supported. The [restrictions reference](./restrictions-and-error-messages.md#run-calls-in-match-guards) describes the error and the rewrite.
:::

## try and catch

A `try` block can contain `.run` calls in its body, and the `catch` clause can contain `.run` calls too:

```scala mdoc
println(runNow(defer {
  try {
    val a = ZIO.succeed(123).run
    val b = ZIO.attempt(somethingUnsafe).run
    a + b
  } catch {
    case e: Exception =>
      ZIO.succeed(789).run
  }
}))
```

Translation:

```scala
ZIO.succeed(123).flatMap { a =>
  ZIO.attempt(somethingUnsafe).map { b =>
    a + b
  }
}.catchAll { case e: Exception =>
  ZIO.succeed(789)
}
```

The `catch` clause receives exceptions that an effect puts into its error channel, such as those from `ZIO.attempt`. An exception thrown by plain code inside `ZIO.succeed` becomes a defect, and the `catch` clause does not see it:

```scala mdoc
println(runNow(defer {
  try {
    ZIO.succeed(throwsException()).run
  } catch {
    case e: Exception => 123
  }
}.exit).isFailure)
```

The `unsafe` block tells `defer` that the code it wraps can throw, so the code is translated into `ZIO.attempt` and the exception is caught:

```scala mdoc
println(runNow(defer {
  try {
    unsafe {
      throwsException()
    }
  } catch {
    case e: Exception => 123
  }
}))
```

Effects with `.run` calls can also appear inside `unsafe`, and they are used as they are:

```scala mdoc
println(runNow(defer {
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
}))
```

A `finally` clause is also supported. It runs after the body, whether or not the body succeeds.

## while

A `while` loop is translated into a recursive function that calls the next iteration after the body finishes. A loop that changes a `var` needs Lenient mode, which the `Use.withLenientCheck` option enables:

```scala mdoc
println(runNow(defer(Use.withLenientCheck) {
  var i = 0
  while (i < 3) {
    ZIO.attempt(println("Hello")).run
    i += 1
  }
  i
}))
```

In the default Strict mode, use a ZIO `Ref` for mutable state, as the [restrictions reference](./restrictions-and-error-messages.md#mutable-variables) explains:

```scala mdoc
println(runNow(defer {
  val ref = Ref.make(0).run
  while (ref.get.run < 3) {
    ZIO.attempt(println("Hello")).run
    ref.update(_ + 1).run
  }
  ref.get.run
}))
```

:::note
ZIO provides `ZIO.iterate` and `ZIO.repeat` for many loops that do not need a `while` statement. Use `while` when the loop condition itself must run effects on each iteration, as in the `Ref` example above.
:::

## for-loop and foreach

A `for` loop over a collection is translated into `ZIO.foreach`. The body contains `.run` calls, and the loop result is `Unit`:

```scala mdoc
println(runNow(defer {
  for (i <- 1 to 3) {
    ZIO.attempt(println(i)).run
  }
}))
```

## Declarations

A `val` can be declared anywhere in a `defer` block. Mutable `var` declarations, `def`, `class`, `lazy val`, and implicit definitions are rejected in the default Strict mode, as the [restrictions reference](./restrictions-and-error-messages.md) describes. The `while` section above shows the Lenient and Ref alternatives.

## Next Steps

- **Want to handle errors in a block?** Read [How to Handle Errors in a defer Block](./how-to-handle-errors.md).
- **Want to change how the block is checked?** Read [Configuration Options for defer](./configuration.md).
