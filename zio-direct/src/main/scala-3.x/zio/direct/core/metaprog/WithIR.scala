package zio.direct.core.metaprog

import scala.quoted._
import pprint._
import fansi.Str
import zio.direct.core.util.Format
import scala.tools.nsc.PipelineMain.Pipeline
import zio.direct.core.util.Unsupported
import zio.direct.core.util.Messages
import zio.direct.core.metaprog.Extractors.BlockN

trait WithIR {
  self: WithF with WithZioType =>

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  /* =============================================== IR =============================================== */
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
    case class ValDef(originalStmt: macroQuotes.reflect.Block, symbol: Symbol, assignment: IR, bodyUsingVal: IR) extends Monadic
    case class Unsafe(body: IR) extends Monadic
    // Use IR.Match.CaseDefs instead of an empty IR.CaseDef list to model a situation with
    // no catch-clause because if it exists, there must be at least one case
    case class Try(tryBlock: IR, catchBlock: Option[IR.Match.CaseDefs], resultType: TypeRepr, finallyBlock: Option[IR]) extends Monadic
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
      def apply(code: Term, source: Monad.Source = Monad.Source.Pipeline) = new Monad(code, source)
      def unapply(value: Monad) = Some(value.code)
      sealed trait Source
      case object Source {
        case object Pipeline extends Source
        // Indicates that this IR.Monad came from a previous Defer clause. This is useful to see inside the tree
        case object PrevDefer extends Source
        case object IgnoreCall extends Source
      }
      private case class Id(code: Term)
    }
    case class Block(head: Statement, tail: Monadic) extends Monadic
    case class Match(scrutinee: IR, caseDefs: IR.Match.CaseDefs, resultType: TypeRepr) extends Monadic
    object Match {
      case class CaseDefs(cases: List[IR.Match.CaseDef])
      case class CaseDef(pattern: Tree, guard: Option[Term], rhs: Monadic)
    }
    case class If(cond: IR, ifTrue: IR, ifFalse: IR) extends Monadic
    case class Pure(code: Term) extends IR with Leaf
    // Note that And/Or expressions ultimately need to have both of their sides lifted,
    // if one either side is not actually a monad we need to lift it. Therefore
    // we can treat And/Or as monadic (i.e. return the from the main Transform)
    case class And(left: IR, right: IR) extends Monadic
    case class Or(left: IR, right: IR) extends Monadic
    case class Parallel(originalExpr: Term, monads: List[(IR.Monadic, Symbol)], body: IR.Leaf) extends Monadic
  }

  /* =============================================== Typed IR (IRT) =============================================== */
  protected sealed trait IRT {
    def zpe: ZioType
  }
  protected object IRT {
    sealed trait Monadic extends IRT
    sealed trait Leaf extends IRT
    case class Fail(error: IRT)(val zpe: ZioType) extends Monadic
    case class While(cond: IRT, body: IRT)(val zpe: ZioType) extends Monadic
    case class ValDef(originalStmt: macroQuotes.reflect.Block, symbol: Symbol, assignment: IRT, bodyUsingVal: IRT)(val zpe: ZioType) extends Monadic
    case class Unsafe(body: IRT)(val zpe: ZioType) extends Monadic
    case class Try(tryBlock: IRT, caseDefs: Option[IRT.Match.CaseDefs], resultType: TypeRepr, finallyBlock: Option[IRT])(val zpe: ZioType) extends Monadic
    case class Foreach(list: IRT, listType: TypeRepr, elementSymbol: Symbol, body: IRT)(val zpe: ZioType) extends Monadic
    case class FlatMap(monad: Monadic, valSymbol: Option[Symbol], body: IRT.Monadic)(val zpe: ZioType) extends Monadic
    case class Map(monad: Monadic, valSymbol: Option[Symbol], body: IRT.Pure)(val zpe: ZioType) extends Monadic
    case class Monad(code: Term, source: IR.Monad.Source)(val zpe: ZioType) extends Monadic with Leaf
    object Monad {
      def fromZioValue(zv: ZioValue) =
        apply(zv.term, IR.Monad.Source.Pipeline)(zv.zpe)
    }

    case class Block(head: Statement, tail: Monadic)(val zpe: ZioType) extends Monadic
    case class Match(scrutinee: IRT, caseDefs: IRT.Match.CaseDefs, resultType: TypeRepr)(val zpe: ZioType) extends Monadic
    case class If(cond: IRT, ifTrue: IRT, ifFalse: IRT)(val zpe: ZioType) extends Monadic
    case class Pure(code: Term)(val zpe: ZioType) extends IRT with Leaf
    object Pure {
      def fromTerm(et: ZioEffectType)(term: Term) = Pure(term)(ZioType.fromPure(et)(term))
    }
    case class And(left: IRT, right: IRT)(val zpe: ZioType) extends Monadic
    case class Or(left: IRT, right: IRT)(val zpe: ZioType) extends Monadic
    case class Parallel(originalExpr: Term, monads: List[(IRT.Monadic, Symbol)], body: IRT.Leaf)(val zpe: ZioType) extends Monadic
    object Match {
      case class CaseDefs(cases: List[IRT.Match.CaseDef])(val zpe: ZioType)
      case class CaseDef(pattern: Tree, guard: Option[Term], rhs: Monadic)(val zpe: ZioType)
    }
  }

  /* =============================================== Various Transforms =============================================== */
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
  object WrapUnsafes {
    def apply[F[_, _, _]: Type, S: Type, W: Type](monad: DirectMonad[F, S, W]) = new WrapUnsafes[F, S, W](monad)
  }
  class WrapUnsafes[F[_, _, _]: Type, S: Type, W: Type](monad: DirectMonad[F, S, W]) extends StatelessTransformer {
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
        pure.code.tpe.asType match
          case '[t] =>
            monad.Failure match
              case Some(monadFailure) =>
                IR.Monad('{ ${ monadFailure }.attempt[t](${ pure.code.asExprOf[t] }) }.asTerm)
              case None =>
                IR.Monad('{ ${ monad.Success }.unit[t](${ pure.code.asExprOf[t] }) }.asTerm)

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
            Unsupported.Error.withTree(origExpr, Messages.UnsafeNotAllowedParallel, InfoBehavior.Info)
          case b @ IR.Block(head, tail) =>
            // basically the only thing that can be inside of a block head-statement is a ValDef
            // or a Term of pure-code. Since val-defs are handled separately as an IR.ValDef basically
            // there should be nothing on than a pure-term in this slot
            val wrappedHead =
              head match {
                case term: Term => monadify(IR.Pure(term))
                case _          => monadify(IR.Pure(macroQuotes.reflect.Block(List(head), '{ () }.asTerm)))
              }
            IR.FlatMap(wrappedHead, None, tail)
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
        case IR.Try(tryBlock, casesOpt, resultType, finallyBlock) =>
          val newCases = casesOpt.map(apply(_))
          val newFinallyBlock = finallyBlock.map(apply(_))
          IR.Try(apply(tryBlock), newCases, resultType, newFinallyBlock)
        case IR.ValDef(orig, symbol, assignment, bodyUsingVal) =>
          IR.ValDef(orig, symbol, apply(assignment), apply(bodyUsingVal))
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
        case IR.Match(scrutinee, caseDefs, rs) =>
          IR.Match(scrutinee, apply(caseDefs), rs)
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

    def apply(caseDefs: IR.Match.CaseDefs): IR.Match.CaseDefs =
      caseDefs.copy(cases = caseDefs.cases.map(apply(_)))
  }
}
