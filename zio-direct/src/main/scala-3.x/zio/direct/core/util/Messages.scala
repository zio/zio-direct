package zio.direct.core.util

// Intentionally put all comments in 1st line. Want this to be a place
// where example code can be put
// format: off
object Messages {

implicit class StringExt(str: String) {
  def trimLeft = str.dropWhile(_.isWhitespace)
}

val ImplicitsNotAllowed =
"""
Implicits are not allowed inside defer clauses (they are allowed inside of `run(...)` blocks.
"""

val MutableCollectionDetected =
"""
Detected the use of a mutable collection inside a defer clause.
Mutable collections can cause many potential issues as a result of defer-clause
rewrites so they are not allowed (Unless it is inside of a run-call).
"""

val DeclarationNotAllowedWithRuns =
"""
In Lenient mode, Class, Function, and Mutable-Variable, and Lazy-Variable definitions are
allowed but only so long as they do not direclty read runs. If you want to assign the
value `run(...)` block to a variable, read into a val first.
=========
Instead of doing somethiing like this:
  defer {
    var i = ZIO.succeed(10).run
    while (i - 2 >= 0) {
      println(s"Currently: $i")
      i = ZIO.succeed(i - 1).run
    }
  }
Consider doing something like this:
  defer {
    var initial = ZIO.succeed(10).run
    var i = initial
    while (i - 2 > 0) {
      println("Value:" + i)
      val update = ZIO.succeed(i - 1).run
      i = update
    }
  }
"""

val RunRemainingAfterTransformer =
"""
Invocations of `run(...)` (or `op.run`) were detected even after all the transformations of zio-direct were completed.
That means that zio-direct cannot successfully process the input you have passed into it. Try to use defer.verbose
to examine the tree structure in order to understand what is wrong or submit a bug-report
at https://github.com/zio/zio-direct.
"""

val UnsafeNotAllowedParallel =
"""
Statements that mix `run(...)` calls with other expressions in the same line are not allowed in
`unsafe { ... }` blocks. Only code that does either a run, or a simple statement is allowed
on each line here. This enhanced restriction is made for the sake of correctness.
For Example:
// Not allowed:
val x = 123 + ZIO.succeed(456).run + 789
// Change it to:
val x0 = ZIO.succeed(456).run
val x = 123 + x0 + 789

There are certain exceptions to this rule such as if there are pure-values
that come only after the effect execution (e.g. `ZIO.succeed(123).run + 456`)
and in such cases compilation will succeed and nothing further needs to be done.
Follow the above rule whenever errors occur.
"""

val MutableAndLazyVariablesNotAllowed =
"""
Mutable and Lazy Variables are not allowed inside of a defer
block unless they are in a ZIO effect inside of a `run` call.
========
For example you cannot do this:
var x = 123
defer {
  val y = x
}
However, you CAN do this:
var x = 123
defer {
  ZIO.succeed { val y = x; y }.run
}
This rule is enfoced for the sake of correctness.
"""

val DeclarationNotAllowed =
"""
Class, Function, and Mutable-Variable. Lazy-Variable definitions
(class X, def X, var X, lazy val X) are not allowed inside of defer blocks unless
they are inside of a ZIO effect in a `run(ZIO)` call.
Please move them outside of the defer area.
"""

val RunAssignmentNotRecommended =
"""
Using Assignment inside of run(...:ZIO) sections is permitted but not recommended,
(outside of an `run` call they are forbidden entirely). Consider using immutable
variables or ZIO Refs if mutability is essential.
=========
Instead of doing somethiing like this:
  defer.verbose {
    var i = ZIO.succeed(10).run
    while (ZIO.succeed(i - 2).run >= 0) {
      println(s"Currently: $i")
      ZIO.succeed { i = i -1 }.run
    }
  }
Consider doing something like this:
  defer.verbose {
    var i = Ref.make(10).run
    while (i.get.run - 2)) > 0) {
      println("Value:" + i.get.run)
      i.getAndUpdate(i => i - 1).run
    }
  }
""".trimLeft

// TODO need to test
val RunInRunError =
"""
An run cannot be inside an run. In order to do this,
write the content of the outer run into a variable first.
=========
For example:
  run(run(ZIO.succeed  { if (foo) ZIO.succeed(x) else ZIO.succeed(y) }))
Change it to:
  val a = run(ZIO.succeed  { if (foo) ZIO.succeed(x) else ZIO.succeed(y) })
  run(a)
""".trimLeft

val AssignmentNotAllowed =
"""
Assignment is generally not allowed inside of defer calls,
because it can cause correctness problems with the
synthesized code if it directly reads the result
of a `run(...)` call or interacts with other
effects in `run(...)` clauses.
Please use a ZIO Ref instead.
=========
For example, instead of this:
defer {
	val i = run(numCalls)
	while (i > 0) {
		println("Value:" + i)
		i = i - 1
	}
}
Do this:
defer.verbose {
  var i = run(Ref.make(10))
  while (run(i.get) - 2) > 0) {
    println("Value:" + run(i.get))
    run(i.getAndUpdate(i => i - 1))
  }
}
"""

val MoveRunOut =
"""
Move the `run` call outside of this structure in order to use it.
=========
For example, change this:
  defer {
    def getUrl = run(httpGet(someUrl))
    service.lookup(getUrl)
  }
To this:
  defer {
    val result = run(httpGet(someUrl))
    def getUrl = result
    service.lookup(getUrl)
  }

In some cases you should move the object out of the defer block entirely.
For example, change this:
  defer {
    def getUrl(url: String) = run(httpGet(url))
    service.lookup(getUrl(someStr))
  }
To this:
  def getUrl(url: String) = httpGet(url)
  defer {
    val result = run(getUrl(someUrl))
    service.lookup(result)
  }
""".trimLeft

}
// format: on
