package zio.run.core.util

// Intentionally put all comments in 1st line. Want this to be a place
// where example code can be put
object Examples {

  implicit class StringExt(str: String) {
    def trimLeft = str.dropWhile(_.isWhitespace)
  }

  val DeclarationNotAllowedWithAwaits =
    """
In Lenient mode, Class, Function, and Mutable-Variable definitions are allowed but only so long as they do not direclty read awaits.
"""

  val DeclarationNotAllowed =
    """
Class, Function, and Mutable-Variable definitions (class X, def X, var X) are not allowed inside of async blocks.
Please move them outside of the async area. (They can be inside of an await)
"""

  val AwaitAssignmentNotRecommended =
    """
Using Assignment inside of await(...:ZIO) sections is permitted but not recommended,
(outside of an `await` call they are forbidden entirely). Consider using ZIO Refs.
=========
Instead of doing somethiing like this:
  async.verbose {
    var i = ZIO.succeed(10).run
    while (ZIO.succeed(i - 2).run >= 0) {
      println(s"Currently: $i")
      ZIO.succeed { i = i -1 }.run
    }
  }
Consider doing something like this:
  async.verbose {
    var i = Ref.make(10).run
    while (i.get.run - 2)) > 0) {
      println("Value:" + i.get.run)
      i.getAndUpdate(i => i - 1).run
    }
  }
""".trimLeft

// TODO need to test
  val AwaitInAwaitError =
    """
An await cannot be inside an await. In order to do this,
write the content of the outer await into a variable first.
=========
For example:
  await(await(ZIO.succeed  { if (foo) ZIO.succeed(x) else ZIO.succeed(y) }))
Change it to:
  val a = await(ZIO.succeed  { if (foo) ZIO.succeed(x) else ZIO.succeed(y) })
  await(a)
""".trimLeft

  val AssignmentNotAllowed =
    """
Assignment is generally not allowed inside of async calls. Please use a ZIO Ref instead.
=========
For example, instead of this:
async {
	val i = await(numCalls)
	while (i > 0) {
		println("Value:" + i)
		i = i - 1
	}
}
Do this:
async.verbose {
  var i = await(Ref.make(10))
  while (await(i.get) - 2) > 0) {
    println("Value:" + await(i.get))
    await(i.getAndUpdate(i => i - 1))
  }
}
"""

  val MoveAwaitOut =
    """
Move the `await` call outside of this structure in order to use it.
=========
For example, change this:
  async {
    def getUrl = await(httpGet(someUrl))
    service.lookup(getUrl)
  }
To this:
  async {
    val result = await(httpGet(someUrl))
    def getUrl = result
    service.lookup(getUrl)
  }

In some cases you should move the object out of the async block entirely.
For example, change this:
  async {
    def getUrl(url: String) = await(httpGet(url))
    service.lookup(getUrl(someStr))
  }
To this:
  def getUrl(url: String) = httpGet(url)
  async {
    val result = await(getUrl(someUrl))
    service.lookup(result)
  }
""".trimLeft

}
