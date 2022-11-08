package zio.asyncawait.core.metaprog

import scala.quoted._
import pprint._
import fansi.Str
import zio.asyncawait.core.util.Format

trait Model {
  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  protected sealed trait IR
  protected object IR {
    sealed trait Monadic extends IR

    case class FlatMap(monad: Monadic, valSymbol: Option[Symbol], body: IR.Monadic) extends Monadic
    object FlatMap {
      def apply(monad: IR.Monadic, valSymbol: Symbol, body: IR.Monadic) =
        new FlatMap(monad, Some(valSymbol), body)
    }
    case class Map(monad: Monadic, valSymbol: Option[Symbol], body: IR.Pure) extends Monadic
    object Map {
      def apply(monad: Monadic, valSymbol: Symbol, body: IR.Pure) =
        new Map(monad, Some(valSymbol), body)
    }
    case class Monad(code: Term) extends Monadic
    // TODO Function to collapse inner blocks into one block because you can have Block(term, Block(term, Block(monad)))
    case class Block(head: Statement, tail: Monadic) extends Monadic

    // TODO scrutinee can be monadic or Match output can be monadic, how about both?
    // scrutinee can be Monadic or Pure. Not using union type so that perhaps can backward-compat with Scala 2
    case class Match(scrutinee: IR, caseDefs: List[IR.Match.CaseDef]) extends Monadic
    object Match {
      case class CaseDef(pattern: Tree, guard: Option[Term], rhs: Monadic)
    }

    // Since we ultimately transform If statements into Task[Boolean] segments, they are monadic
    // TODO during transformation, decided cases based on if ifTrue/ifFalse is monadic or not
    case class If(cond: IR, ifTrue: IR, ifFalse: IR) extends Monadic
    case class Pure(code: Term) extends IR

    // Note that And/Or expressions ultimately need to have both of their sides lifted,
    // if one either side is not actually a monad we need to lift it. Therefore
    // we can treat And/Or as monadic (i.e. return the from the main Transform)
    case class And(left: IR, right: IR) extends Monadic
    case class Or(left: IR, right: IR) extends Monadic

    case class Parallel(monads: List[(Term, Symbol)], body: Term) extends Monadic
  }
}
