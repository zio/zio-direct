---
id: restrictions-and-error-messages
title:  "Restrictions and Error Messages"
sidebar_label: "Restrictions and Errors"
description: "Reference for the constructs that zio-direct rejects inside a defer block, the compiler messages that report them, and how to rewrite each rejected construct."
keywords:
  - "ZIO Direct Style"
  - "defer"
  - "error messages"
  - "restrictions"
  - "Strict mode"
---

Some Scala constructs cannot appear inside a `defer` block, because zio-direct cannot rewrite them into `flatMap` chains without changing their meaning. When you use one of them, the compiler reports an error that names the problem. This page lists the restrictions, the first line of each message, and the rewrite that usually fixes it.

## Mutable Variables

The examples on this page import the ZIO and zio-direct APIs:

```scala mdoc:silent
import zio._
import zio.direct._
```

A `var` declaration is rejected in the default Strict mode. The compiler reports the first line below, followed by a list of flags that describe the declaration:

```scala mdoc:fail
val counter: Task[Int] =
  defer {
    var count = 0
    count
  }
```

To keep a changing value, use a ZIO `Ref`, which is a mutable cell that every `.run` call can update:

```scala mdoc:silent
val counterRef: Task[Int] =
  defer {
    val count = Ref.make(0).run
    count.update(_ + 1).run
    count.get.run
  }
```

Lenient mode accepts `var` declarations. See the [configuration reference](./configuration.md) for the modes.

## Function and Class Definitions

A `def`, a `class`, or a `lazy val` declared inside a `defer` block is rejected in Strict mode, with the messages `Function Definitions are not allowed` and `Lazy Variable Definitions are not allowed`. Move the definition above the `defer` block, and pass any values it needs as parameters.

## Implicit Values

An `implicit` or `given` value defined inside a `defer` block is rejected with the message `Implicits are not allowed inside defer clauses`. Define the implicit outside the block. Implicit parameters of a function are allowed.

## Run Calls in Match Guards

A `.run` call inside a `case` guard is not supported. A guard such as `case x if ZIO.succeed(x).run > 0 =>` is left in the code, so zio-direct reports that a `run` call remains after the rewrite. Bind the value before the `match` instead:

```scala
defer {
  val n = ZIO.succeed(5).run
  n match {
    case x if x > 0 => "positive"
    case _          => "not positive"
  }
}
```

## Mutable Collections

A value of a mutable collection type, such as `scala.collection.mutable.ArrayBuffer`, is rejected when it appears directly as a statement outside a `.run` call. The message reports the collection name and suggests wrapping the expression in an effect:

```scala
defer {
  ArrayBuffer(1, 2, 3) // rejected: wrap it in ZIO.attempt(...).run
}
```

## Assignment

An assignment to a variable outside a `.run` call is rejected with the message `Assignment is generally not allowed inside of defer calls`. Use a `Ref` instead, as shown in the mutable variables section. An assignment inside a `.run` call compiles, and the compiler reports a warning that assignment there is not recommended.

## Mixing Run Calls With Other Expressions

Inside an `unsafe { ... }` block, a line cannot mix a `.run` call with other expressions. The message is `Statements that mix run(...) calls with other expressions in the same line are not allowed`. Move the `.run` call into its own `val`:

```scala
defer {
  val a = ZIO.succeed(123).run
  val total = a + 456
  total
}
```

## Unhandled Exceptions Inside Plain Code

Plain Scala code that throws an exception inside a `defer` block is not caught by `try`, because `defer` treats it as a pure computation. Wrap the code in `unsafe { ... }` to make it part of the effect. The [error-handling guide](./how-to-handle-errors.md) shows the full pattern.

## Messages That Mention run(...)

Some messages still show the older form `run(...)` in their examples. Both forms compile, and the `.run` suffix is the preferred form in new code. When a message shows `run(...)`, read it as the `.run` call in the same position.

## Constructs That Work

The following constructs are accepted inside a `defer` block: `val`, `if`, `match` (with `.run` in the scrutinee and in the case bodies), `try` with `catch` and `finally`, and nested `defer` blocks. Loops such as `while` and `for` are accepted too, and a `while` loop that changes a `var` needs Lenient mode. The [supported constructs reference](./supported-constructs.md) gives an example of each.

## What You've Learned

- Mutable state, definitions, and implicits are rejected in Strict mode, with a message that names each construct
- Guards cannot contain `.run` calls, and mutable collections must be wrapped in an effect
- Each rejected construct has a rewrite that keeps the effects in `.run` calls

## Where to Go Next

- **Want to see each supported construct?** Read [Scala Constructs Supported by ZIO-Direct](./supported-constructs.md).
- **Want to change the check mode?** Read [Configuration Options for defer](./configuration.md).
- **Stuck on a message?** Read [How to Debug a defer Block](./how-to-debug-defer.md).
