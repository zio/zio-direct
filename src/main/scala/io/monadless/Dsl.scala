package io.monadless

import zio.Task
import scala.quoted._
import io.monadless.core.Transformer
import zio.ZIO

def await[T](value: ZIO[Any, Throwable, T]): T = ???

inline def async[T](inline value: T): Task[T] = ${ Dsl.liftImpl[T]('value) }

object Dsl {
  def liftImpl[T: Type](value: Expr[T])(using Quotes): Expr[ZIO[Any, Throwable, T]] = {
    (new Transformer).apply(value)
  }
}