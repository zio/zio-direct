package zio.direct.core.metaprog

import scala.quoted._
import pprint._
import fansi.Str
import zio.direct.core.util.Format
import zio.ZIO
import scala.tools.nsc.PipelineMain.Pipeline
import zio.direct.core.util.Unsupported
import zio.direct.core.util.Messages

trait WithIR {
  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  protected sealed trait IR
  protected object IR {
    sealed trait Monadic extends IR
    sealed trait Leaf extends IR {
      def code: Term
    }

    // TODO It's possible to do `throw run(function)` so need to support IR
    // being passed into a throw.
    case class Fail(error: IR) extends Monadic

    case class While(cond: IR, body: IR) extends Monadic

    case class Unsafe(body: IR) extends Monadic

    case class Try(tryBlock: IR, cases: List[IR.Match.CaseDef], resultType: TypeRepr, finallyBlock: Option[IR]) extends Monadic

    case class Foreach(list: IR, listType: TypeRepr, elementSymbol: Symbol, body: IR) extends Monadic

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

    class Monad private (val code: Term, val source: Monad.Source) extends Monadic with Leaf {
      private val id = Monad.Id(code)
      override def equals(other: Any): Boolean =
        other match {
          case v: Monad => id == v.id
          case _        => false
        }
    }
    object Monad {
      def apply(code: Term, source: Monad.Source = Monad.Source.Pipeline) =
        new Monad(code, source)

      def unapply(value: Monad) =
        Some(value.code)

      sealed trait Source
      case object Source {
        case object Pipeline extends Source
        // Indicates that this IR.Monad came from a previous Defer clause. This is useful to see inside the tree
        case object PrevDefer extends Source
        case object IgnoreCall extends Source
      }

      private case class Id(code: Term)
    }

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
    case class Pure(code: Term) extends IR with Leaf

    // Note that And/Or expressions ultimately need to have both of their sides lifted,
    // if one either side is not actually a monad we need to lift it. Therefore
    // we can treat And/Or as monadic (i.e. return the from the main Transform)
    case class And(left: IR, right: IR) extends Monadic
    case class Or(left: IR, right: IR) extends Monadic

    case class Parallel(originalExpr: Term, monads: List[(IR.Monadic, Symbol)], body: IR.Leaf) extends Monadic
  }

  /**
   * Wrap the IR.Pures/IR.Maps in the body of IR.Unsafe elements with ZIO.attempt.
   * This is largely for the sake of tries working as expected, for example.
   * (assume all the above examples are wrapped in `defer { ... }`)
   *   try { 1/0 } catch { case e: DivideByZeroException => ... }
   * Normally with zio-run this will not be caught because 1/0 will not be wrapped into a ZIO.attempt.
   * This statement will become
   *   succeed(1/0).catchSome { case e: ... }
   * Instead we need it to become
   *   attempt(1/0).catchSome { case e: ... }
   * This phase will do that.
   *
   * Also in a statement like this
   *  try { val x = run(foo); 1/0 } catch { case e: ... }
   * It will become
   *  run(foo).map(x => 1/0).catchSome { case e: ... }
   * Instead we need it to become
   *  run(foo).flatMap(x => attempt(1/0)).catchSome { case e: ... }
   *
   * Most interestingly, in a statement like this
   *   try { unsafe { (run(foo), 4/0, run(bar)) } } catch { case: e...  }
   * Normally it would turn into
   *   { collect(Chunk(foo, bar)).map(iter => { val par1 = iter.next; var par2 = iter.next; (par1, 4/0, bar) }
   * We need it to become
   *   { collect(Chunk(foo, bar)).flatMap(iter => attempt { val par1 = iter.next; var par2 = iter.next; (par1, 4/0, bar) }
   */
  object WrapUnsafes extends StatelessTransformer {
    override def apply(ir: IR.Monadic): IR.Monadic =
      ir match
        case IR.Unsafe(body) =>
          // we can actually remove the IR.Unsafe at that point but it is still useful
          // as a marker of where we did those operations.
          IR.Unsafe(MakePuresIntoAttemps(body))
        case _ =>
          super.apply(ir)

    def makePuresIntoAttemps(i: IR) =
      MakePuresIntoAttemps(i)

    private object MakePuresIntoAttemps extends StatelessTransformer {
      private def monadify(pure: IR.Pure) =
        IR.Monad('{ ZIO.attempt(${ pure.code.asExpr }) }.asTerm)

      // Monadify all top-level pure calls
      override def apply(ir: IR): IR =
        ir match
          case v: IR.Pure => monadify(v)
          case _          => super.apply(ir)

      // Monadify pure calls inside IR.Leaf instances (inside IR.Parallel)
      override def apply(ir: IR.Leaf): IR.Leaf =
        ir match
          case v: IR.Pure  => monadify(v)
          case v: IR.Monad => v

      override def apply(ir: IR.Monadic): IR.Monadic =
        ir match
          case IR.Map(monad, valSymbol, pure) => IR.FlatMap(apply(monad), valSymbol, monadify(pure))
          case v @ IR.Parallel(origExpr, monads, body) =>
            Unsupported.Error.withTree(origExpr, Messages.ParallelNotAllowedInRun, InfoBehavior.Info)
          case _ => super.apply(ir)
    }
  }

  trait StatelessTransformer {
    def apply(ir: IR): IR =
      ir match
        case v: IR.Pure    => apply(v)
        case v: IR.Monadic => apply(v)

    def apply(ir: IR.Pure): IR.Pure = ir
    def apply(ir: IR.Monad): IR.Monad = ir
    // Specifically used in this like IR.Parallel that can have either a Pure or Monad element
    // but either way it has to be a leaf node (i.e. can't have structures inside)
    def apply(ir: IR.Leaf): IR.Leaf = ir

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
        case IR.Foreach(list, listType, symbolType, body) =>
          IR.Foreach(apply(list), listType, symbolType, apply(body))
        case IR.Map(monad, valSymbol, body) =>
          IR.Map(apply(monad), valSymbol, apply(body))
        case IR.Fail(error) => IR.Fail(apply(error))
        case v: IR.Monad    => apply(v)
        case IR.Block(head, tail) =>
          IR.Block(head, apply(tail))
        case IR.Match(scrutinee, caseDefs) =>
          val newCaseDefs = caseDefs.map(apply(_))
          IR.Match(scrutinee, newCaseDefs)
        case IR.If(cond, ifTrue, ifFalse) => IR.If(cond, apply(ifTrue), apply(ifFalse))
        case IR.And(left, right)          => IR.And(apply(left), apply(right))
        case IR.Or(left, right)           => IR.Or(apply(left), apply(right))
        case IR.Parallel(orig, monads, body) =>
          val newMonads = monads.map((monad, sym) => (apply(monad), sym))
          val newBody = apply(body)
          IR.Parallel(orig, newMonads, newBody)
        case IR.Unsafe(body) =>
          IR.Unsafe(body)

    def apply(caseDef: IR.Match.CaseDef): IR.Match.CaseDef =
      val newRhs = apply(caseDef.rhs)
      caseDef.copy(rhs = newRhs)
  }
}
