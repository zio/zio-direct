package zio.direct.core.metaprog

//import zio.direct.core.util.Format
//import zio.direct.core.util.Unsupported
import scala.reflect.macros.whitebox.Context
import zio.ZIO
import zio.direct.core.metaprog.Trees
import scala.annotation.nowarn
import zio.direct.core.util.Messages
import zio.direct.core.util.WithUnsupported

trait MacroBase {
  val c: Context
  type Uni = c.universe.type
  // NOTE: u needs to be lazy otherwise sets value from c before c can be initialized by higher level classes
  lazy val u: Uni = c.universe

  import c.universe._

  // Make this pass a term instead of a tree Type since Scala2 does not differentiate
  // between Trees that Terms and just Statements, the .tpe of a statement will just be null
  // which is a very unsafe behavior. Instead, for the user to pass in the tree that can
  // be checked 1st to see if it is a term.
  def isTermZIO(term: Tree) =
    term.isTerm && term.tpe <:< typeOf[ZIO[Any, Any, Any]] && !(term.tpe =:= typeOf[Nothing])

  object SymbolExt {
    def isSynthetic(s: Symbol) = isSyntheticName(getName(s))
    private def isSyntheticName(name: String) = {
      name == "<init>" || (name.startsWith("<local ") && name.endsWith(">")) || name == "$anonfun"
    }
    private def getName(s: Symbol) = s.name.decodedName.toString.trim
  }

  implicit class SymbolOps(sym: Symbol) {
    def isSynthetic = sym.isSynthetic || SymbolExt.isSynthetic(sym)
    def isMutableVariable = sym.isTerm && sym.asTerm.isVar
  }

  object report {
    def errorAndAbort(msg: String) = c.abort(c.enclosingPosition, msg)
    def errorAndAbort(msg: String, tree: Tree) = c.abort(tree.pos, msg)

    def warning(msg: String) = c.warning(c.enclosingPosition, msg)
    def warning(msg: String, tree: Tree) = c.warning(tree.pos, msg)
    def warning(msg: String, pos: Position) = c.warning(pos, msg)

    def info(msg: String) = c.info(c.enclosingPosition, msg, true)
    def info(msg: String, pos: Position) = c.info(pos, msg, true)
    def info(msg: String, tree: Tree) = c.info(tree.pos, msg, true)
  }

  object PureTree {
    object All {
      def unapply(trees: List[Tree]): Boolean =
        trees.forall(PureTree.unapply(_).isDefined)
    }

    def unapply(tree: Tree): Option[Tree] =
      Trees.exists(c)(tree) {
        case RunCall(_)    => true
        case Try(_, _, _)  => true
        case UnsafeCall(_) => true
        case Throw(_)      => true
        case _             => false
      } match {
        case true  => None
        case false => Some(tree)
      }
  }

  def is[T](tpe: Type)(implicit t: TypeTag[T]) =
    tpe <:< t.tpe

  def isA[T](tree: Tree)(implicit t: TypeTag[T]) =
    tree.tpe <:< t.tpe

  def toVal(name: TermName) = q"val $name = $EmptyTree"
  def wildcard = TermName("_")
  def freshName(x: String = "x") = TermName(c.freshName(x))

  def useNewSymbolIn(tpe: Type)(useSymbol: Tree => Tree) = {
    val termName = freshName("m")
    val ref = q"$termName"
    (termName, useSymbol(ref))
  }

  class ApplyZioDirectMethod(methodName: String) {
    private val fullMethodName = s"zio.direct.${methodName}"
    def unapply(tree: Tree) =
      tree match {
        case Apply(TypeApply(runTree, tpes), List(v)) if (runTree.symbol.isMethod && runTree.symbol.fullName == fullMethodName) =>
          Some((v, tpes.map(_.tpe)))
        case _ =>
          None
      }
  }

  object DeferredCall {
    private val applyDeferredMethod = new ApplyZioDirectMethod("Internal.deferred")
    def unapply(tree: Tree): Option[Tree] =
      tree match {
        case applyDeferredMethod(v, _) => Some(v)
        case _                         => None
      }
  }

  object UnsafeCall {
    private val applyUnsafeMethod = new ApplyZioDirectMethod("unsafe")
    def unapply(tree: Tree): Option[Tree] =
      tree match {
        case applyUnsafeMethod(v, _) => Some(v)
        case _                       => None
      }
  }

  object IgnoreCall {
    private val applyIgnoreMethod = new ApplyZioDirectMethod("Internal.ignore")
    def unapply(tree: Tree): Option[Tree] =
      tree match {
        case applyIgnoreMethod(v, _) => Some(v)
        case _                       => None
      }
  }

  object RunCall {
    private val applyRunMethod = new ApplyZioDirectMethod("run")
    def unapply(tree: Tree): Option[Tree] = {
      tree match {
        // Can't just do "case q"$pack.run[..$tpes]($v)" because if the `run` method is renamed it won't find that
        case applyRunMethod(v, _) => Some(v)
        // Technically if the extension method .run is renamed somehow this won't match. I'm not sure if doing that
        // is actually possible so will keep the quasiquoted solution for now.
        case q"$pack.ZioRunOps[..$tpes]($v).run" => Some(v)
        case _                                   => None
      }
    }
  }

  object RunCallWithType {
    private val applyRunMethod = new ApplyZioDirectMethod("run")
    def unapply(tree: Tree): Option[(Tree, Type)] =
      tree match {
        case applyRunMethod(v, tpes)             => Some((v, tpes.last))
        case q"$pack.ZioRunOps[..$tpes]($v).run" => Some((v, tpes.last.tpe))
        case _                                   => None
      }
  }

  object ZioApply {

    private def computeCodeInsertion(code: Tree) = {
      // if code is an assignment e.g. `x = x + 1` and it is passed
      // as a function into ZIO.succeed it will become ZIO.succeed(x = x + 1)
      // at which point Scala2 macros will thing that x is a parameter of ZIO.succeed
      // in this case we need to forcibly make that into a block. Same with
      // situations where the statement is a non-term (things like e.g. `import._` would be PureTree, so they wouldn't match this condition)
      // but I am not sure if these cases could actually occur.
      // (NOTE That the above problem even occurs if you put the code snippets into a list and do `{...${List(code)}}` )
      code match {
        case _ if (!code.isTerm) =>
          val codes = List(code)
          q"{ ..$codes; () }"
        case asi: Assign =>
          val codes = List(code)
          q"{ (); ..$codes }"
        case other =>
          other
      }
    }

    def succeed(code: Tree) =
      q"""zio.ZIO.succeed(${computeCodeInsertion(code)})"""

    def attempt(code: Tree) =
      q"zio.ZIO.attempt(${computeCodeInsertion(code)})"

    def True =
      q"zio.ZIO.succeed(true)"
    def False =
      q"zio.ZIO.succeed(false)"
  }

  object ValDefStatement {
    def unapply(tree: Tree): Option[(TermName, Tree)] =
      tree match {
        case q"$mods val $name: $t = $rhs" => Some((name, rhs))
        case _                             => None
      }
  }

  object IsTerm {
    def unapply(tree: Tree): Option[Tree] =
      if (tree.isTerm) Some(tree) else None
  }

  object BlockN {
    def unapply(trees: List[Tree]) =
      trees match {
        case Nil => None
        case IsTerm(head) :: Nil =>
          Some(Block(Nil, head))
        case list if (IsTerm.unapply(list.last).isDefined) =>
          Some(Block(list.dropRight(1), IsTerm.unapply(list.last).get))
        case _ =>
          report.errorAndAbort(s"Last element in the instruction group is not a block. ${trees.map(show(_))}")
      }

    def apply(trees: List[Tree]): Block =
      BlockN.unapply(trees) match {
        case Some(value) => value
        case None        => report.errorAndAbort(s"Invalid trees list: ${trees.map(show(_))}")
      }
  }
}

trait WithIR extends MacroBase {
  self: WithUnsupported with WithZioType =>

  import c.universe._

  sealed trait IR
  // can't be sealed or type-checking on IR.Monadic causes this: https://github.com/scala/bug/issues/4440
  object IR {
    sealed trait Monadic extends IR
    sealed trait Leaf extends IR {
      def code: c.universe.Tree
    }

    case class Fail(error: IR) extends Monadic

    case class While(cond: IR, body: IR) extends Monadic

    case class ValDef(originalStmt: Tree /*should be a block*/, symbol: TermName, assignment: IR, bodyUsingVal: IR) extends Monadic

    case class Unsafe(body: IR) extends Monadic

    case class Try(tryBlock: IR, cases: List[IR.Match.CaseDef], resultType: c.universe.Type, finallyBlock: Option[IR]) extends Monadic

    case class Foreach(list: IR, listType: c.universe.Type, elementSymbol: TermName, body: IR) extends Monadic

    case class FlatMap(monad: Monadic, valSymbol: Option[TermName], body: IR.Monadic) extends Monadic
    object FlatMap {
      def apply(monad: IR.Monadic, valSymbol: TermName, body: IR.Monadic) =
        new FlatMap(monad, Some(valSymbol), body)
    }
    case class Map(monad: Monadic, valSymbol: Option[TermName], body: IR.Pure) extends Monadic
    object Map {
      def apply(monad: Monadic, valSymbol: TermName, body: IR.Pure) =
        new Map(monad, Some(valSymbol), body)
    }

    class Monad private (val code: c.universe.Tree, val source: Monad.Source) extends Monadic with Leaf {
      private val id = Monad.Id(code)
      override def equals(other: Any): Boolean =
        other match {
          case v: Monad => id == v.id
          case _        => false
        }
    }
    object Monad {
      def apply(code: c.universe.Tree, source: Monad.Source = Monad.Source.Pipeline) =
        new Monad(code, source)

      def unapply(value: Monad) =
        Some(value.code)

      sealed trait Source
      case object Source {
        case object Pipeline extends Source
        case object PrevDefer extends Source
        case object IgnoreCall extends Source
      }

      private case class Id(code: c.universe.Tree)
    }

    case class Block(head: c.universe.Tree, tail: Monadic) extends Monadic
    case class Match(scrutinee: IR, caseDefs: List[IR.Match.CaseDef]) extends Monadic
    object Match {
      case class CaseDef(pattern: Tree, guard: Option[c.universe.Tree], rhs: Monadic)
    }
    case class If(cond: IR, ifTrue: IR, ifFalse: IR) extends Monadic
    case class Pure(code: c.universe.Tree) extends IR with Leaf
    case class And(left: IR, right: IR) extends Monadic
    case class Or(left: IR, right: IR) extends Monadic

    case class Parallel(originalExpr: c.universe.Tree, monads: List[(IR.Monadic, TermName, Type)], body: IR.Leaf) extends Monadic
  }

  // Typed version of IR AST
  sealed trait IRT
  object IRT {
    sealed trait Monadic extends IRT

    sealed trait Leaf extends IRT {
      def code: c.universe.Tree
    }

    case class Fail(error: IRT)(val zpe: ZioType) extends Monadic

    case class While(cond: IRT, body: IRT)(val zpe: ZioType) extends Monadic

    case class ValDef(originalStmt: Tree /*should be a block*/, symbol: TermName, assignment: IRT, bodyUsingVal: IRT)(val zpe: ZioType) extends Monadic

    case class Unsafe(body: IRT)(val zpe: ZioType) extends Monadic

    case class Try(tryBlock: IRT, cases: List[IR.Match.CaseDef], resultType: c.universe.Type, finallyBlock: Option[IRT])(val zpe: ZioType) extends Monadic

    case class Foreach(list: IRT, listType: c.universe.Type, elementSymbol: TermName, body: IRT)(val zpe: ZioType) extends Monadic

    case class FlatMap(monad: Monadic, valSymbol: Option[TermName], body: IRT.Monadic)(val zpe: ZioType) extends Monadic

    object FlatMap {
      def apply(monad: IRT.Monadic, valSymbol: TermName, body: IRT.Monadic)(zpe: ZioType) =
        new FlatMap(monad, Some(valSymbol), body)(zpe)
    }

    case class Map(monad: Monadic, valSymbol: Option[TermName], body: IRT.Pure)(val zpe: ZioType) extends Monadic

    object Map {
      def apply(monad: Monadic, valSymbol: TermName, body: IRT.Pure)(zpe: ZioType) =
        new Map(monad, Some(valSymbol), body)(zpe)
    }

    class Monad private (val code: c.universe.Tree, val source: Monad.Source)(val zpe: ZioType) extends Monadic with Leaf {
      private val id = Monad.Id(code)

      override def equals(other: Any): Boolean =
        other match {
          case v: Monad => id == v.id
          case _        => false
        }
    }

    object Monad {
      def apply(code: c.universe.Tree, source: Monad.Source = Monad.Source.Pipeline)(zpe: ZioType) =
        new Monad(code, source)(zpe)

      def unapply(value: Monad) =
        Some(value.code)

      sealed trait Source

      case object Source {
        case object Pipeline extends Source

        case object PrevDefer extends Source

        case object IgnoreCall extends Source
      }

      private case class Id(code: c.universe.Tree)
    }

    case class Block(head: c.universe.Tree, tail: Monadic)(val zpe: ZioType) extends Monadic

    case class Match(scrutinee: IRT, caseDefs: List[IRT.Match.CaseDef])(val zpe: ZioType) extends Monadic

    object Match {
      case class CaseDef(pattern: Tree, guard: Option[c.universe.Tree], rhs: Monadic)
    }

    case class If(cond: IRT, ifTrue: IRT, ifFalse: IRT)(val zpe: ZioType) extends Monadic

    case class Pure(code: c.universe.Tree)(val zpe: ZioType) extends IRT with Leaf

    case class And(left: IRT, right: IRT)(val zpe: ZioType) extends Monadic

    case class Or(left: IRT, right: IRT)(val zpe: ZioType) extends Monadic

    case class Parallel(originalExpr: c.universe.Tree, monads: List[(IRT.Monadic, TermName, Type)], body: IRT.Leaf)(val zpe: ZioType) extends Monadic
  }

  object WrapUnsafes extends StatelessTransformer {
    override def apply(ir: IR.Monadic): IR.Monadic =
      ir match {
        case IR.Unsafe(body) =>
          // we can actually remove the IR.Unsafe at that point but it is still useful
          // as a marker of where we did those operations.
          IR.Unsafe(MakePuresIntoAttemps(body))
        case _ =>
          super.apply(ir)
      }

    def makePuresIntoAttemps(i: IR) =
      MakePuresIntoAttemps(i)

    private object MakePuresIntoAttemps extends StatelessTransformer {
      private def monadify(pure: IR.Pure) =
        IR.Monad(ZioApply.attempt(pure.code))

      // Monadify all top-level pure calls
      override def apply(ir: IR): IR =
        ir match {
          case v: IR.Pure => monadify(v)
          case _          => super.apply(ir) // // //
        }

      // Monadify pure calls inside IR.Leaf instances (inside IR.Parallel)
      override def apply(ir: IR.Leaf): IR.Leaf =
        ir match {
          case v: IR.Pure  => monadify(v)
          case v: IR.Monad => v
        }

      override def apply(ir: IR.Monadic): IR.Monadic =
        ir match {
          case IR.Map(monad, valSymbol, pure) => IR.FlatMap(apply(monad), valSymbol, monadify(pure))
          case v @ IR.Parallel(origExpr, monads, body) =>
            Unsupported.Error.withTree(origExpr, Messages.UnsafeNotAllowedParallel, InfoBehavior.Info)
          case b @ IR.Block(head, tail) =>
            // basically the only thing that can be inside of a block head-statement is a ValDef
            // or a Term of pure-code. Since val-defs are handled separately as an IR.ValDef basically
            // there should be nothing on than a pure-term in this slot
            val wrappedHead =
              if (head.isTerm)
                monadify(IR.Pure(head))
              else
                monadify(IR.Pure(c.universe.Block(List(head), q"()")))

            IR.FlatMap(wrappedHead, None, tail)
          case _ => super.apply(ir)
        }
    }
  }

  trait StatelessTransformer {
    @nowarn
    def apply(ir: IR): IR =
      ir match {
        case v: IR.Pure => apply(v)
        // since there are no multiple instances of WithIR I don't think the outer reference issue maters
        case v: IR.Monadic => apply(v)
      }

    def apply(ir: IR.Pure): IR.Pure = ir
    def apply(ir: IR.Monad): IR.Monad = ir
    // Specifically used in this like IR.Parallel that can have either a Pure or Monad element
    // but either way it has to be a leaf node (i.e. can't have structures inside)
    def apply(ir: IR.Leaf): IR.Leaf = ir

    def apply(ir: IR.Monadic): IR.Monadic =
      ir match {
        case IR.While(cond, body) =>
          IR.While(apply(cond), apply(body))
        case IR.Try(tryBlock, cases, resultType, finallyBlock) =>
          val newCases = cases.map(apply(_))
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
        case IR.Match(scrutinee, caseDefs) =>
          val newCaseDefs = caseDefs.map(apply(_))
          IR.Match(scrutinee, newCaseDefs)
        case IR.If(cond, ifTrue, ifFalse) => IR.If(cond, apply(ifTrue), apply(ifFalse))
        case IR.And(left, right)          => IR.And(apply(left), apply(right))
        case IR.Or(left, right)           => IR.Or(apply(left), apply(right))
        case IR.Parallel(orig, monads, body) =>
          val newMonads = monads.map { case (monad, sym, tpe) => (apply(monad), sym, tpe) }
          val newBody = apply(body)
          IR.Parallel(orig, newMonads, newBody)
        case IR.Unsafe(body) =>
          IR.Unsafe(body)
      }

    def apply(caseDef: IR.Match.CaseDef): IR.Match.CaseDef = {
      val newRhs = apply(caseDef.rhs)
      caseDef.copy(rhs = newRhs)
    }
  }
}
