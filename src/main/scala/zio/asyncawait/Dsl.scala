package zio.asyncawait

import zio.Task
import scala.quoted._
import zio.asyncawait.core.Transformer
import zio.ZIO
import zio.asyncawait.core.metaprog.MakeInstructions
import zio.asyncawait.core.metaprog.Instructions
import zio.asyncawait.core.metaprog.InfoBehavior

def await[R, E, A](value: ZIO[R, E, A]): A = ???

object async {
  transparent inline def apply[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.applyImpl[T]('value) }
  transparent inline def info[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.infoImpl[T]('value) }
  transparent inline def verbose[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.verboseImpl[T]('value) }
}


object Dsl {
  def verboseImpl[T: Type](value: Expr[T])(using q: Quotes): Expr[ZIO[?, ?, ?]] = {
    (new Transformer(q)).apply(value, Instructions(InfoBehavior.Verbose))
  }

  def infoImpl[T: Type](value: Expr[T])(using q: Quotes): Expr[ZIO[?, ?, ?]] = {
    (new Transformer(q)).apply(value, Instructions(InfoBehavior.Info))
  }

  def applyImpl[T: Type](value: Expr[T])(using q: Quotes): Expr[ZIO[?, ?, ?]] = {
    (new Transformer(q)).apply(value, Instructions(InfoBehavior.Silent))
  }
}