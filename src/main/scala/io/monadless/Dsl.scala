package io.monadless

import zio.Task
import scala.quoted._
import io.monadless.core.Transformer

def unlift[T](value: Task[T]): T = ???

inline def lift[T](inline value: T): Task[T] = ${ Dsl.liftImpl[T]('value) }

object Dsl {
  def liftImpl[T: Type](value: Expr[T])(using Quotes): Expr[Task[T]] = {
    (new Transformer).apply(value)
  }
}