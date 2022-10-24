package zio.asyncawait

import zio.Task
import scala.quoted._
import zio.asyncawait.core.Transformer
import zio.ZIO

def await[T](value: ZIO[Any, Throwable, T]): T = ???

inline def async[T](inline value: T): Task[T] = ${ Dsl.liftImpl[T]('value) }

object Dsl {
  def liftImpl[T: Type](value: Expr[T])(using Quotes): Expr[ZIO[Any, Throwable, T]] = {
    (new Transformer).apply(value)
  }
}