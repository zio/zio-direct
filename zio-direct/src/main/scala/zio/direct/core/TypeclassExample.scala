// package zio.direct.core

// case class Foo(valueReal: String)

// trait ValueAble[T] {
//   inline def value(inline t: T): String
// }

// object FooValueAble extends ValueAble[Foo] {
//   inline def value(inline t: Foo): String = t.valueReal + "-Add"
// }

// transparent implicit inline def valueAbleFoo: ValueAble[Foo] = FooValueAble
// transparent implicit inline def valueAbleFooReal: FooValueAble.type = FooValueAble
// transparent inline def useInstance[T](inline t: T)(implicit inline instance: ValueAble[T]) = instance.value(t)

// import scala.quoted._

// object UseValueAble {
//   transparent inline def apply[T, VA <: ValueAble[T]](inline t: T)(implicit inline va: VA): String = ${ impl[T, VA]('t, 'va) }
//   def impl[T: Type, VA <: ValueAble[T]: Type](t: Expr[T], va: Expr[VA])(using Quotes): Expr[String] = {
//     import quotes.reflect._
//     // val summoned = Expr.summon[ValueAble[T]].getOrElse { report.errorAndAbort("Cannot summon") }
//     // println(s"Summoned: ${summoned.asTerm.tpe.show}.")
//     // val expr = '{ valueAbleFooReal }.asExprOf[ValueAble[Foo]]
//     // val expr = '{ valueAbleFooReal.value(Foo("other")) }
//     // '{ $expr.value(Foo("blah")) }
//     val output = '{ useInstance(Foo("blahblahbla"))(FooValueAble) }
//     // val output = '{ $va.value($t) }
//     println(s"Code: ${output.asTerm.underlyingArgument.asExpr.show}")
//     output
//   }
// }
