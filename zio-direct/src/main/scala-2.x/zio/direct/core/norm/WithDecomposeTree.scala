package zio.direct.core.norm

import zio.direct.core.metaprog._
import scala.collection.mutable
//import zio.direct.core.norm.WithComputeType
//import zio.direct.core.norm.WithReconstructTree
//import zio.direct.core.util.Unsupported
import zio.direct.core.util.WithFormat
import zio.direct.core.util.WithUnsupported

trait WithDecomposeTree extends MacroBase {
  self: WithIR with WithFormat with WithUnsupported =>
  import c.universe._

  protected object Decompose {
    def apply(instr: Instructions) =
      new Decompose(instr)
  }

  protected class Decompose(instr: Instructions) {
    def apply(value: Tree) = DecomposeTree.orPure(value)

    private object DecomposeTree {
      def orPure(term: Tree): IR =
        unapply(term).getOrElse(IR.Pure(term))

      def orPure2(termA: Tree, termB: Tree): (IR, IR) =
        (unapply(termA).getOrElse(IR.Pure(termA)), unapply(termB).getOrElse(IR.Pure(termB)))

      def unapply(expr: Tree): Option[IR.Monadic] = {
        val ret = expr match {
          // TODO Make sure it's dsl.unsafe i.e. the pattern is right
          case q"$pack.dsl.Internal.deferred($effect)" =>
            Some(IR.Monad(effect, IR.Monad.Source.PrevDefer))

          case block @ Block(parts, lastPart) =>
            DecomposeBlock.unapply(block)

          case PureTree(tree) =>
            None

          case DecomposeSingleTermConstruct(monad) => Some(monad)

          case If(cond, ifTrue, ifFalse) =>
            val (ifTrueIR, ifFalseIR) = DecomposeTree.orPure2(ifTrue, ifFalse)
            val condIR = DecomposeTree.orPure(cond)
            Some(IR.If(condIR, ifTrueIR, ifFalseIR))

          // Not supporting do-while because Scala 3 does not support it
          case q"while($cond) $body" =>
            Some(IR.While(DecomposeTree.orPure(cond), DecomposeTree.orPure(body)))

          case q"$list.foreach(($alias) => $expr)" if isA[Iterable[Any]](list) =>
            val monad = DecomposeTree.orPure(list)
            val body = DecomposeTree.orPure(expr)
            Some(IR.Foreach(monad, list.tpe, alias.name, body))

          case q"$a && $b" if (isA[Boolean](a) && isA[Boolean](b)) =>
            val (aTerm, bTerm) = DecomposeTree.orPure2(a, b)
            Some(IR.And(aTerm, bTerm))

          case q"$a || $b" if (isA[Boolean](a) && isA[Boolean](b)) =>
            val (aTerm, bTerm) = DecomposeTree.orPure2(a, b)
            Some(IR.Or(aTerm, bTerm))

          case Match(m @ DecomposeTree(monad), caseDefs) =>
            val (oldSymbol, body) =
              useNewSymbolIn(m.tpe)(sym => Match(sym, caseDefs))

            val out =
              body match {
                case DecomposeTree(bodyMonad) =>
                  IR.FlatMap(monad, oldSymbol, bodyMonad)
                case bodyPure =>
                  IR.Map(monad, oldSymbol, IR.Pure(bodyPure))
              }
            Some(out)

          case m @ Match(value, DecomposeCases(cases)) =>
            Some(IR.Match(IR.Pure(value), cases))

          case RunCall(task) =>
            Some(IR.Monad(task))

          case Typed(tree, _) =>
            unapply(tree)

          // This is perhaps the most powerful component of the monadless-paradigm. It changes something that looks like this:
          // { (unlift(foo), unlift(bar)) }
          // To something that looks like:
          // { ZIO.collect(foo, bar).map(iter => val a = iter.next(); val b = iter.next(); (a, b)) }
          case term => // @ Allowed.ParallelExpression()
            val unlifts = mutable.ArrayBuffer.empty[(IR.Monadic, TermName)]
            val newTree: Tree =
              Trees.Transform(c)(term) {
                case originalTerm @ RunCall(task) =>
                  val sym = freshName("runVal")
                  unlifts += ((IR.Monad(task), sym))
                  q"$sym"

                case originalTerm @ DecomposeSingleTermConstruct(monad) =>
                  val sym = freshName("singleVal")
                  unlifts += ((monad, sym))
                  q"$sym"

                case originalTerm @ DecomposeBlock(monad) =>
                  // Take the type from the originalTerm (i.e. the result of the run call since it could be a block etc...)
                  val sym = freshName("blockVal")
                  unlifts += ((monad, sym))
                  q"$sym"
              }
            Some(IR.Parallel(term, unlifts.toList, IR.Pure(newTree)))
        }
        ret
      }
    }

    /**
     * DecomposeTree a sequence of steps
     * a; b = unlift(zio); c
     * Into a.flatMap()
     */
    private object DecomposeBlock {
      def unapply(block: Block): Option[IR.Monadic] = {
        val (head, tail) =
          block match {
            case Block(head, tail) => (head, tail)
            case _                 => Unsupported.Error.withTree(block, "Unsupported block", InfoBehavior.default)
          }
        val parts = head :+ tail
        parts match {
          // This is the most important use-case of the monadless system.
          // Change this:
          //   val x = unlift(stuff)
          //   stuff-that-uses-x
          // Into this:
          //   stuff.flatMap(x => ...)
          //   stuff-that-uses-x
          //
          // This basically does that with some additional details
          // (e.g. it can actually be stuff.flatMap(v => val x = v; stuff-that-uses-x))
          // TODO A zero-args DefDef (i.e. a ByName can essentially be treated as a ValDef so we can use that too)

          // A ValDef statement is basically a block because it consists of
          //   {
          //   val x = run(monad)
          //   otherStuff
          //   }
          // The entire above block is represented by the valdef:
          //   IR.ValDef({val x...}, x, monad, otherStuff)
          case (origStmt @ ValDefStatement(varName, assignmentTerm)) :: tail =>
            println(s"------ Tpe: ${origStmt.tpe.typeSymbol.getClass.getName()}")
            println(s"------ Term: ${origStmt.tpe.termSymbol.name.getClass.getName()}")
            println(s"------ TermName: ${TermName("foo").getClass().getName()}")
            val assignment =
              assignmentTerm match {
                case DecomposeTree(monad) => monad
                case _                    => IR.Pure(assignmentTerm)
              }
            val restOfBlock: Block = BlockN(origStmt +: tail)
            val out =
              BlockN(tail) match {
                case DecomposeTree(monadBody) =>
                  IR.ValDef(restOfBlock, varName, assignment, monadBody)
                case pureBody =>
                  IR.ValDef(restOfBlock, varName, assignment, IR.Pure(pureBody))
              }
            Some(out)

          // If the rest of the structure is a pure tree then exit
          case Block(PureTree.All(), PureTree(_)) =>
            None

          case DecomposeTree(monad) :: tail =>
            tail match {
              // In this case where is one statement in the block which my definition
              // needs to have the same type as the output: e.g.
              //   val v: T = { unlift(doSomething:ZIO[_, _, T]) }
              // since we've pulled out the `doSomething` inside the signature
              // will be ZIO[_, _, T] instead of T.
              case Nil =>
                Some(monad)
              case list =>
                // In this case there are multiple instructions inside the seauence e.g:
                //   val v: T = { unlift(x), y /*pure*/, unlift(z:ZIO[_, _, T]) }
                // We recurse by flatMapping x, y, and unlift... but eventually the type
                // also has to be ZIO[_, _, T] since we are essentially doing:
                // x.flatMap(.. -> {y; z: ZIO[_, _, T]}). Of course the value will be ZIO[_, _, T]
                // since the last value of a nested flatMap chain is just the last instruction
                // in the nested sequence.
                val out =
                  BlockN(tail) match {
                    case DecomposeTree(bodyMonad) =>
                      IR.FlatMap(monad, None, bodyMonad)
                    case bodyPure =>
                      IR.Map(monad, None, IR.Pure(bodyPure))
                  }
                Some(out)
            }

          // This is the recursive case of DecomposeBlock, it will work across multiple things
          // between blocks due to the recursion e.g:
          //   val blah = new Blah(2) // 1st part, will recurse 1st time (here)
          //   import blah._          // 2nd part, will recurse 2nd time (here)
          //   val b = unlift(ZIO.succeed(value).asInstanceOf[Task[Int]]) // Then will match valdef case
          case head :: BlockN(DecomposeBlock(parts)) =>
            Unsupported.Warn.checkUnmooredZio(head)
            Some(IR.Block(head, parts))

          case other =>
            None
        }
      }
    }

    private object DecomposeSingleTermConstruct {
      def unapply(term: Tree) =
        term match {
          // TODO Make sure it's dsl.unsafe i.e. the pattern is right
          case q"$pack.dsl.unsafe($value)" =>
            Some(IR.Unsafe(DecomposeTree.orPure(value)))

          // If we have a special user-defined "ignore" block, just splice the code. The `ignore` construct
          // is should ONLY be used to test code.
          // TODO Make sure it's dsl.unsafe i.e. the pattern is right
          case q"$pack.dsl.ignore($code)" =>
            if (isZIO(code.tpe))
              Some(IR.Monad(code, IR.Monad.Source.IgnoreCall))
            else
              Some(IR.Monad(ZioApply.succeed(code)))

          case tryTerm @ Try(tryBlock, caseDefs, finallyBlock) =>
            val tryBlockIR = DecomposeTree.orPure(tryBlock)
            val cases = DecomposeCases(caseDefs)
            val finallyBlockOpt =
              if (!finallyBlock.isEmpty)
                Some(DecomposeTree.orPure(finallyBlock))
              else
                None
            Some(IR.Try(tryBlockIR, cases, tryTerm.tpe, finallyBlockOpt))

          case q"throw $e" =>
            Some(IR.Fail(DecomposeTree.orPure(e)))

          case _ => None
        }
    }

    private object DecomposeCases {
      def apply(cases: List[CaseDef]): List[IR.Match.CaseDef] =
        applyMark(cases)

      private def applyMark(cases: List[CaseDef]) =
        cases.map {
          case CaseDef(pattern, cond, DecomposeTree(body)) =>
            val condOpt = if (!cond.isEmpty) Some(cond) else None
            IR.Match.CaseDef(pattern, condOpt, body)
          case CaseDef(pattern, cond, body) =>
            val condOpt = if (!cond.isEmpty) Some(cond) else None
            IR.Match.CaseDef(pattern, condOpt, IR.Monad(ZioApply.succeed(body)))
          case other =>
            Unsupported.Error.withTree(other, "Invalid Case-Def", InfoBehavior.default)
        }

      def unapply(cases: List[CaseDef]) =
        // If at least one of the match-cases need to be transformed, transform all of them
        Some(applyMark(cases))
    }
  }
}
