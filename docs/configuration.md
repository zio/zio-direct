---
id: configuration
title:  "Configuration Options for defer"
sidebar_label: "Configuration"
description: "Reference for the Use options and defer variants that change how zio-direct checks and evaluates a defer block: Strict, Lenient, and None modes, parallel evaluation, and linear mode."
keywords:
  - "ZIO Direct Style"
  - "defer"
  - "Use"
  - "Strict mode"
  - "Lenient mode"
---

Every `defer` block accepts an optional `Use` value that changes how zio-direct checks the block and evaluates its `.run` calls. Without one, the block uses the default settings: strict checks and sequential evaluation. The sections below list each option and the situation it is for.

## Use Options

Pass a `Use` option as the first argument of `defer`, and the block takes the option's behavior. Each option is a method on `Use`:

| Option                      | Check mode | Evaluation   | Purpose                                                  |
| --------------------------- | ---------- | ------------ | -------------------------------------------------------- |
| `Use.withLenientCheck`      | Lenient    | Sequential   | Allows pure `var` declarations and mutation inside the block |
| `Use.withNoCheck`           | None       | Sequential   | Skips the mutability and declaration checks              |
| `Use.withParallelEval`      | Strict     | Parallel     | Runs the `.run` calls of one expression at the same time |
| `Use.withAbstractError`     | Strict     | Sequential   | Infers the least common supertype of the error types     |

Chain options with dots to combine them, such as `Use.withLenientCheck.withParallelEval`.

## Strict Mode

Strict mode is the default. It rejects mutable state inside a `defer` block, because assignment can cause correctness problems with the code that zio-direct generates. A `var` declaration fails with the message `Mutable Variable Definitions are not allowed`:

```scala
defer {
  var count = 0 // fails in Strict mode
  count
}
```

Strict mode also rejects `class`, `def`, and `lazy val` definitions with similar messages. To keep state in a `defer` block, use a ZIO `Ref`, as shown on the [constructs page](./supported-constructs.md).

## Lenient Mode

Lenient mode allows `var` declarations and mutation as long as they do not read the result of a `.run` call. Enable it with `Use.withLenientCheck`. The next example uses a `var` to count the loop iterations:

```scala mdoc:silent
import zio._
import zio.direct._

val counted: Task[Int] =
  defer(Use.withLenientCheck) {
    var count = 0
    while (count < 3) {
      ZIO.succeed(()).run
      count += 1
    }
    count
  }
```

Running the code below prints the result:

```scala mdoc
println(Unsafe.unsafe { implicit u => Runtime.default.unsafe.run(counted).getOrThrowFiberFailure() })
```

The code above:

- `var count = 0` is allowed because Lenient mode does not reject it
- `count += 1` changes the variable inside the loop
- the loop runs three times, and the block returns `3`

Lenient mode still rejects a `var` whose initial value comes from a `.run` call, such as `var i = ZIO.succeed(10).run`. Read the value into a `val` first, then copy it into the `var`.

## None Mode

`Use.withNoCheck` turns off the mutability and declaration checks entirely. zio-direct still reports leftover `.run` calls that it could not rewrite. Use this option only when you have verified that the block's mutable state cannot interact with the effects it runs, because nothing else protects the rewrite.

## Parallel Evaluation

`Use.withParallelEval` runs the `.run` calls that belong to one expression, such as the elements of a tuple, at the same time. The default is sequential evaluation, where each `.run` waits for the previous one. The option is covered step by step in the [parallel how-to guide](./how-to-run-in-parallel.md).

## Least Upper Bound Error Types

`Use.withAbstractError` changes how zio-direct computes the error type of a `defer` block that combines effects with different errors. Instead of a union type such as `FooError | BarError`, the block uses the least upper bound of the errors. Use it when your IDE cannot display union types, as described in the [IntelliJ guide](./intellij-support.md#caveats). The option is not fully supported for error types that contain type parameters.

## Linear Mode

`defer.linear` is a variant of `defer` for Scala 3. It always uses Lenient checks, and it rejects any line that mixes a `.run` call with other expressions. Use it when you want each effect to appear on its own line:

```scala mdoc:silent
val linearResult: Task[Int] =
  defer.linear {
    val first = ZIO.succeed(20).run
    val second = ZIO.succeed(22).run
    first + second
  }
```

Running the code below prints the result:

```scala mdoc
println(Unsafe.unsafe { implicit u => Runtime.default.unsafe.run(linearResult).getOrThrowFiberFailure() })
```

The code above:

- each `val` contains exactly one `.run` call, which linear mode requires
- `first + second` contains no `.run` call, so it is allowed on its own line
- the block returns `42`

The `Use` type has no option that restores Strict checks, so a linear block always runs with Lenient checks.

## Inspecting the Translation

The `defer.info`, `defer.verbose`, and `defer.verboseTree` variants print the code that zio-direct generates at compile time. They take the same `Use` options. Their use is described in the [debugging guide](./how-to-debug-defer.md), once it is written.
