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
In Lenient mode, Class, Function, and Mutable-Variable definitions are allowed but only so long as they do not direclty read runs.
"""

val RunRemainingAfterTransformer =
"""
Invocations of `run(...)` (or `op.run`) were detected even after all the transformations of zio-direct were completed.
That means that zio-direct cannot successfully process the input you have passed into it. Try to use defer.verbose
to examine the tree structure in order to understand what is wrong or submit a bug-report
at https://github.com/zio/zio-direct.
"""

val DeclarationNotAllowed =
"""
Class, Function, and Mutable-Variable definitions (class X, def X, var X) are not allowed inside of defer blocks unless they in the `run` call.
Please move them outside of the defer area. (They can be inside of an run)
"""

val RunAssignmentNotRecommended =
"""
Using Assignment inside of run(...:ZIO) sections is permitted but not recommended,
(outside of an `run` call they are forbidden entirely). Consider using ZIO Refs.
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
as it can cause serious rewrite-correctness issues if
if it directly reads the result of a `run(...)` call.
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
