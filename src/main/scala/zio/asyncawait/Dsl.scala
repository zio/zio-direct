package zio.asyncawait

import zio.Task
import scala.quoted._
import zio.asyncawait.core.Transformer
import zio.ZIO

def await[R, E, A](value: ZIO[R, E, A]): A = ???

transparent inline def async[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.liftImpl[T]('value) }

object Dsl {
  def liftImpl[T: Type](value: Expr[T])(using Quotes): Expr[ZIO[?, ?, ?]] = {
    (new Transformer).apply(value)
  }
}