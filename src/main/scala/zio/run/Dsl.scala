package zio.run

import zio.Task
import scala.quoted._
import zio.run.core.Transformer
import zio.ZIO
import zio.run.core.metaprog.Instructions
import zio.run.core.metaprog.InfoBehavior
import zio.run.core.metaprog.Collect
import zio.run.core.metaprog.Unliftables
import zio.run.core.metaprog.Verify
import zio.run.core.NotDeferredException

def await[R, E, A](value: ZIO[R, E, A]): A = NotDeferredException.fromNamed("await")
def unsafe[T](value: T): T = NotDeferredException.fromNamed("unsafe")

object async {
  transparent inline def apply[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Silent }, '{ Collect.Sequence }, '{ Verify.Strict }) }
  transparent inline def info[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Info }, '{ Collect.Sequence }, '{ Verify.Strict }) }
  transparent inline def verbose[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Verbose }, '{ Collect.Sequence }, '{ Verify.Strict }) }
  transparent inline def verboseTree[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.VerboseTree }, '{ Collect.Sequence }, '{ Verify.Strict }) }

  transparent inline def apply[T](inline collect: Collect = Collect.Sequence, inline verify: Verify = Verify.Strict)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Silent }, 'collect, 'verify) }
  transparent inline def info[T](inline collect: Collect = Collect.Sequence, inline verify: Verify = Verify.Strict)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Info }, 'collect, 'verify) }
  transparent inline def verbose[T](inline collect: Collect = Collect.Sequence, inline verify: Verify = Verify.Strict)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Verbose }, 'collect, 'verify) }
  transparent inline def verboseTree[T](inline collect: Collect = Collect.Sequence, inline verify: Verify = Verify.Strict)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.VerboseTree }, 'collect, 'verify) }
}

extension [R, E, A](inline value: ZIO[R, E, A])
  transparent inline def run = await(value)

object Dsl {
  import InfoBehavior._

  def impl[T: Type](value: Expr[T], infoBehavior: Expr[InfoBehavior], col: Expr[Collect], verify: Expr[Verify])(using q: Quotes): Expr[ZIO[?, ?, ?]] =
    doTransform(value, Unliftables.unliftInfoBehavior(infoBehavior), Unliftables.unliftCollect(col), Unliftables.unliftVerify(verify))

  def doTransform[T: Type](value: Expr[T], infoBehavior: InfoBehavior, collect: Collect, verify: Verify)(using q: Quotes): Expr[ZIO[?, ?, ?]] =
    (new Transformer(q)).apply(value, Instructions(infoBehavior, collect, verify))
}
