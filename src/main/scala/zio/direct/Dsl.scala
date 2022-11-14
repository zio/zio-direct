package zio.direct

import zio.Task
import scala.quoted._
import zio.direct.core.Transformer
import zio.ZIO
import zio.direct.core.metaprog.Instructions
import zio.direct.core.metaprog.InfoBehavior
import zio.direct.core.metaprog.Collect
import zio.direct.core.metaprog.Unliftables
import zio.direct.core.metaprog.Verify
import zio.direct.core.NotDeferredException

def unsafe[T](value: T): T = NotDeferredException.fromNamed("unsafe")

object defer {
  transparent inline def apply[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Silent }, '{ Collect.Sequence }, '{ Verify.Strict }) }
  transparent inline def info[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Info }, '{ Collect.Sequence }, '{ Verify.Strict }) }
  transparent inline def verbose[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Verbose }, '{ Collect.Sequence }, '{ Verify.Strict }) }
  transparent inline def verboseTree[T](inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.VerboseTree }, '{ Collect.Sequence }, '{ Verify.Strict }) }

  transparent inline def apply[T](inline collect: Collect = Collect.Sequence, inline verify: Verify = Verify.Strict)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Silent }, 'collect, 'verify) }
  transparent inline def info[T](inline collect: Collect = Collect.Sequence, inline verify: Verify = Verify.Strict)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Info }, 'collect, 'verify) }
  transparent inline def verbose[T](inline collect: Collect = Collect.Sequence, inline verify: Verify = Verify.Strict)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.Verbose }, 'collect, 'verify) }
  transparent inline def verboseTree[T](inline collect: Collect = Collect.Sequence, inline verify: Verify = Verify.Strict)(inline value: T): ZIO[?, ?, ?] = ${ Dsl.impl[T]('value, '{ InfoBehavior.VerboseTree }, 'collect, 'verify) }
}

extension [R, E, A](value: ZIO[R, E, A]) {
  def run: A = NotDeferredException.fromNamed("run")
}

object Dsl {
  import InfoBehavior._

  def impl[T: Type](value: Expr[T], infoBehavior: Expr[InfoBehavior], col: Expr[Collect], verify: Expr[Verify])(using q: Quotes): Expr[ZIO[?, ?, ?]] =
    doTransform(value, Unliftables.unliftInfoBehavior(infoBehavior), Unliftables.unliftCollect(col), Unliftables.unliftVerify(verify))

  def doTransform[T: Type](value: Expr[T], infoBehavior: InfoBehavior, collect: Collect, verify: Verify)(using q: Quotes): Expr[ZIO[?, ?, ?]] =
    (new Transformer(q)).apply(value, Instructions(infoBehavior, collect, verify))
}
