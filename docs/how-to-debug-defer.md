---
id: how-to-debug-defer
title:  "How to Debug a defer Block"
sidebar_label: "Debug defer Blocks"
description: "Print the code that zio-direct generates for a defer block with defer.info and defer.verbose, and use the output to find why a block does not compile or behaves unexpectedly."
keywords:
  - "ZIO Direct Style"
  - "defer"
  - "debugging"
  - "defer.info"
  - "defer.verbose"
---

Use this guide when a `defer` block does not compile, or when it runs in a way you did not expect. The debugging variants of `defer` print the code that zio-direct generates, so you can compare it with the code you wrote.

## Print the Generated Code With defer.info

`defer.info` works like `defer`, and it also prints the code that zio-direct generates at compile time. Use it on the block you want to inspect:

```scala
import zio._
import zio.direct._

def loadUser(id: Int): Task[String] = ZIO.attempt(s"user-$id")
def loadTheme(user: String): Task[String] = ZIO.succeed(s"dark-theme-for-$user")

val themed: Task[String] =
  defer.info {
    val user = loadUser(42).run
    val theme = loadTheme(user).run
    s"$user uses $theme"
  }
```

For the block above, the compiler prints the following code. It is copied from a compile of that block, so the object names such as `MdocApp` come from the docs build and differ in your project:

```scala
ZioMonad.Success.flatMap(MdocApp.this.loadUser(42))(((sm: String) => {
  val user: String = sm
  ZioMonad.Success.map(MdocApp.this.loadTheme(user))(((`sm₂`: String) => {
    val theme: String = `sm₂`
    _root_.scala.StringContext.apply("", " uses ", "").s(user, theme)
  }))
}))
```

Compare the printed code with the block: each `val` that contains a `.run` call becomes a `flatMap` or `map` step, and the last expression is returned from the innermost step. If a `.run` call appears where you did not expect it, the printed code shows where zio-direct placed it.

:::tip
`defer.info` prints during compilation, so the output appears in the compiler log and not in the page. Run the compile again after changing the block to see the new translation.
:::

## Show More Detail With defer.verbose

`defer.verbose` prints the same translation as `defer.info`, along with the type that zio-direct computed for each step and the tree it deconstructed from your code. Use it when the translation looks right but the inferred type is not:

```scala
val themedVerbose: Task[String] =
  defer.verbose {
    val user = loadUser(42).run
    val theme = loadTheme(user).run
    s"$user uses $theme"
  }
```

The output is long, so start with `defer.info` and move to `defer.verbose` only when you need the types.

## Check Errors Before Changing the Block

A failed `defer` block shows a message that names the construct that zio-direct rejected. The [supported constructs reference](./supported-constructs.md) lists the constructs that are allowed inside `defer`, and the [configuration reference](./configuration.md) explains how the Strict, Lenient, and None check modes change which constructs pass.

Do not use `Use.withNoCheck` to silence an error during debugging. That option skips the checks, so the block may compile and still behave differently from what you intend.

## What You've Learned

- `defer.info` prints the generated code during compilation
- `defer.verbose` adds the computed types and the deconstructed tree
- Restrictions are fixed by changing the block, not by disabling checks

## Where to Go Next

- **Want the full list of constructs?** Read [Scala Constructs Supported by ZIO-Direct](./supported-constructs.md).
- **Want to handle failures in a block?** Read [How to Handle Errors in a defer Block](./how-to-handle-errors.md).
