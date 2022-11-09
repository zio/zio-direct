package zio.asyncawait

import zio.Task
import scala.quoted._
import zio.asyncawait.core.Transformer
import zio.ZIO
import zio.asyncawait.core.metaprog.Instructions
import zio.asyncawait.core.metaprog.InfoBehavior
import zio.asyncawait.core.metaprog.Collect
import zio.asyncawait.core.metaprog.Unliftables

def await[R, E, A](value: ZIO[R, E, A]): A = ???

object async {
  transparent inline def apply[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{InfoBehavior.Silent}, '{Collect.Sequence}) }
  transparent inline def info[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{InfoBehavior.Info}, '{Collect.Sequence}) }
  transparent inline def verbose[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{InfoBehavior.Verbose}, '{Collect.Sequence}) }
  transparent inline def verboseTree[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{InfoBehavior.VerboseTree}, '{Collect.Sequence}) }

  transparent inline def apply[T](inline collect: Collect)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{InfoBehavior.Silent}, 'collect) }
  transparent inline def info[T](inline collect: Collect)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{InfoBehavior.Info}, 'collect) }
  transparent inline def verbose[T](inline collect: Collect)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{InfoBehavior.Verbose}, 'collect) }
  transparent inline def verboseTree[T](inline collect: Collect)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{InfoBehavior.VerboseTree}, '{Collect.Sequence}) }
}

extension [R, E, A](inline value: ZIO[R, E, A])
  transparent inline def run = await(value)


object Dsl {
  import InfoBehavior._

  def impl[T: Type](value: Expr[T], infoBehavior: Expr[InfoBehavior], col: Expr[Collect])(using q: Quotes): Expr[ZIO[?, ?, ?]] =
    doTransform(value, Unliftables.unliftInfoBehavior(infoBehavior), Unliftables.unliftCollect(col))

  def doTransform[T: Type](value: Expr[T], infoBehavior: InfoBehavior, collect: Collect)(using q: Quotes): Expr[ZIO[?, ?, ?]] =
    (new Transformer(q)).apply(value, Instructions(infoBehavior, collect))
}