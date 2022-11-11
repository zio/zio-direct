package zio.asyncawait.core.metaprog

import scala.quoted._
import pprint._
import fansi.Str
import zio.asyncawait.core.util.Format
import zio.ZIO

trait Model {
  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  protected sealed trait IR
  protected object IR {
    sealed trait Monadic extends IR

    case class While(cond: IR, body: IR) extends Monadic

    case class Try(tryBlock: IR, cases: List[IR.Match.CaseDef], resultType: TypeRepr, finallyBlock: Option[IR]) extends Monadic

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

  object MonadifyTries extends StatelessTransformer {
    override def apply(ir: IR): IR =
      ir match
        case IR.Try(tryBlock, cases, resultType, finallyBlock) =>
          val newTryBlock = makePuresIntoAttemps(tryBlock)
          IR.Try(newTryBlock, cases, resultType, finallyBlock)
        case _ =>
          super.apply(ir)

    def makePuresIntoAttemps(i: IR) =
      MakePuresIntoAttemps(i)

    private object MakePuresIntoAttemps extends StatelessTransformer {
      private def monadify(pure: IR.Pure) =
        IR.Monad('{ ZIO.attempt(${pure.code.asExpr}) }.asTerm)

      override def apply(ir: IR): IR =
        ir match
          case v: IR.Pure =>
            monadify(v)
          case IR.Map(monad, valSymbol, pure) =>
            IR.FlatMap(apply(monad), valSymbol, monadify(pure))
          case _ =>
            // recurse and modify all Pure's to become monadic
            super.apply(ir)

      // needed or the `apply(monad)` becomes ambigous?
      override def apply(ir: IR.Monadic): IR.Monadic =
        super.apply(ir)
    }
  }

  trait StatelessTransformer {
    def apply(ir: IR): IR =
      ir match
        case v: IR.Pure => apply(v)
        case v: IR.Monadic => apply(v)

    def apply(ir: IR.Pure): IR.Pure = ir

    def apply(ir: IR.Monadic): IR.Monadic =
      ir match
        case IR.While(cond, body) =>
          IR.While(apply(cond), apply(body))
        case IR.Try(tryBlock, cases, resultType, finallyBlock) =>
          val newCases = cases.map(apply(_))
          val newFinallyBlock = finallyBlock.map(apply(_))
          IR.Try(apply(tryBlock), newCases, resultType, newFinallyBlock)
        case IR.FlatMap(monad, valSymbol, body) =>
          IR.FlatMap(apply(monad), valSymbol, apply(body))
        case IR.Map(monad, valSymbol, body) =>
          IR.Map(apply(monad), valSymbol, apply(body))
        case v: IR.Monad => v
        case IR.Block(head, tail) =>
          IR.Block(head, apply(tail))
        case IR.Match(scrutinee, caseDefs) =>
          val newCaseDefs = caseDefs.map(apply(_))
          IR.Match(scrutinee, newCaseDefs)
        case IR.If(cond, ifTrue, ifFalse) => IR.If(cond, apply(ifTrue), apply(ifFalse))
        case IR.And(left, right) => IR.And(apply(left), apply(right))
        case IR.Or(left, right) => IR.Or(apply(left), apply(right))
        case v: IR.Parallel => v

    def apply(caseDef: IR.Match.CaseDef): IR.Match.CaseDef =
      val newRhs = apply(caseDef.rhs)
      caseDef.copy(rhs = newRhs)
  }
}
