package zio.asyncawait.core

import scala.quoted._
import zio.Task
import zio.asyncawait.core.metaprog.Extractors._
import zio.asyncawait.core.metaprog._
import zio.asyncawait._
import zio.asyncawait.core.util.Format
import zio.ZIO
import scala.collection.mutable
import zio.Chunk
import zio.asyncawait.core.util.PureTree
import zio.asyncawait.core.util.ComputeTotalZioType

class Transformer(using transformerQuotes: Quotes) {
  import quotes.reflect._

  private sealed trait IR
  private object IR {
    sealed trait Monadic extends IR
    case class FlatMap(monad: Monadic, valSymbol: Option[Symbol], body: IR) extends Monadic
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
    case class If(cond: IR.Bool, ifTrue: IR, ifFalse: IR) extends Monadic
    case class Pure(code: Term) extends IR

    sealed trait Bool extends IR
    object Bool {
      // Note that And/Or expressions ultimately need to have both of their sides lifted,
      // if one either side is not actually a monad we need to lift it. Therefore
      // we can treat And/Or as monadic (i.e. return the from the main Transform)
      case class And(left: IR, right: IR) extends Bool with Monadic
      case class Or(left: IR, right: IR) extends Bool with Monadic
      case class Pure(code: Term) extends Bool
    }
  }


  private def replaceSymbolIn(in: Term)(oldSymbol: Symbol, newSymbolTerm: Term) =
    BlockN(List(
      ValDef(oldSymbol, Some(newSymbolTerm)),
      in
    ))

  def useNewSymbolIn(using Quotes)(tpe: quotes.reflect.TypeRepr)(useSymbol: quotes.reflect.Term => quotes.reflect.Term) = {
    import quotes.reflect._
    // TODO Try to write this using ValDef.let(...). Might be more efficient
    val (symbol, body) =
      tpe.asType match
        case '[t] =>
          // TODO get rid of underlyingArgument. Should only need one top-level Uninline
          '{ val m: t = ???; ${useSymbol(('m).asTerm).asExpr} }.asTerm.underlyingArgument match
            case Block(
              (valdef @ ValDef(_, _, _)) :: Nil,
              body
            ) =>
              (valdef.symbol, body)

    (symbol, body)
  }

  private object Transform {
    def apply(term: Term): IR.Monadic =
      unapply(term).getOrElse(IR.Monad('{ ZIO.succeed(${term.asExpr}) }.asTerm))

    // TODO really use underlyingArgument????
    def unapply(expr: Term): Option[IR.Monadic] = {
      val ret = expr match {
        case PureTree(tree) =>
          None

        case If(cond, ifTrue, ifFalse) =>
          cond match
            case Transform(monad) =>
              val sym = Symbol.newVal(Symbol.spliceOwner, "ifVar", TypeRepr.of[Boolean], Flags.EmptyFlags, Symbol.noSymbol)
              val body = If(Ref(sym), ifTrue, ifFalse)
              Some(IR.FlatMap(monad, sym, IR.Monad(body)))
            case _ =>
              (ifTrue, ifFalse) match {
                case (Transform(ifTrue), Transform(ifFalse)) =>
                  Some(IR.If(IR.Bool.Pure(cond), ifTrue, ifFalse))
                case (Transform(ifTrue), ifFalse) =>
                  Some(IR.If(IR.Bool.Pure(cond), ifTrue, IR.Monad('{ ZIO.succeed(${ifFalse.asExpr}) }.asTerm)))
                case (ifTrue, Transform(ifFalse)) =>
                  Some(IR.If(IR.Bool.Pure(cond), IR.Monad('{ ZIO.succeed(${ifTrue.asExpr}) }.asTerm), ifFalse))
                case (ifTrue, ifFalse) =>
                  None
              }

        case Seal('{ ($a: Boolean) && ($b: Boolean) }) =>
          (a.asTerm, b.asTerm) match {
            case (Transform(a), Transform(b)) =>
              Some(IR.Bool.And(a, b))
            case (Transform(a), b) =>
              Some(IR.Bool.And(a, IR.Bool.Pure(b)))
            case (a, Transform(b)) =>
              Some(IR.Bool.And(IR.Bool.Pure(a), b))
            // case (a, b) is handled by the PureTree case
          }

        case Seal('{ ($a: Boolean) || ($b: Boolean) }) =>
          (a.asTerm, b.asTerm) match {
            case (Transform(a), Transform(b)) =>
              Some(IR.Bool.Or(a, b))
            case (Transform(a), b) =>
              Some(IR.Bool.Or(a, IR.Bool.Pure(b)))
            case (a, Transform(b)) =>
              Some(IR.Bool.And(IR.Bool.Pure(a), b))
            // case (a, b) is handled by the PureTree case
          }

        case block @ Block(parts, lastPart) if (parts.nonEmpty) =>
          TransformBlock.unapply(block)

        case Match(m @ Transform(monad), caseDefs) =>
          // Since in Scala 3 we cannot just create a arbitrary symbol and pass it around.
          // (See https://github.com/lampepfl/dotty/blob/33818506801c80c8c73649fdaab3782c052580c6/library/src/scala/quoted/Quotes.scala#L3675)
          // In order to be able to have a valdef-symbol to manipulate, we need to create the actual valdef
          // Therefore we need to create
          // a synthetic val-def for the symbol with which to substitute this expression.
          // For example if we want to substitute something like this:
          //   unlift(ZIO.attempt(stuff)) match { case ...can-use-m.... }
          // We need to do something like this:
          //   ZIO.attempt(stuff).map(m => match { case ...can-use-m.... })
          // However, in order to be able to get the `m => ...`
          // We first need to create a fake val-def + right-hand-side that looks like:
          //   '{ val m:StuffType = ???; ...can-use-m.... }
          // So the Nest-call can actually change it to:
          //   ZIO.attempt(stuff).map(m => match { case ...can-use-m.... })
          val (oldSymbol, body) =
            useNewSymbolIn(m.tpe)(sym => Match(sym, caseDefs))

          val out =
            body match
              case Transform(bodyMonad) =>
                IR.FlatMap(monad, oldSymbol, bodyMonad)
              case bodyPure =>
                IR.Map(monad, oldSymbol, IR.Pure(bodyPure))
          Some(out)

        case m @ Match(value, TransformCases(cases)) =>
          Some(IR.Match(IR.Pure(value), cases))

        case Seal('{ await[r, e, a]($task) }) =>
          Some(IR.Monad(task.asTerm))

        case Typed(tree, _) =>
          unapply(tree)

        // Change something that looks like this:
        // { (unlift(foo), unlift(bar)) }
        // To something that looks like:
        // { ZIO.collect(foo, bar).map(iter => val a = iter.next(); val b = iter.next(); (a, b)) }
        // TODO MAJOR Only support parallel awaits for strict-set of known constructs
        //      Examine set of cases where this occurs. Then as a final case
        //      Check that there are awaits in any other kind of construct
        //      and throw an unsupproted-construct exception.
        case term => // @ Allowed.ParallelExpression()
          println(s"=============== Other: ${Format(Printer.TreeShortCode.show(term))}")
          val unlifts = mutable.ArrayBuffer.empty[(Term, Symbol, TypeRepr)]
          val newTree: Term =
            Trees.Transform(term, Symbol.spliceOwner) {
              case Seal('{ await[r, e, a]($task) }) =>
                val tpe =
                  task.asTerm.tpe.asType match
                    case '[ZIO[x, y, t]] => TypeRepr.of[t]
                // (unlift(A), unlift(B))
                // Would yield
                // (newSymA:Symbol, Ident(newSymA)), (newSymA:Symbol, Ident(newSymB))
                // The idents would be swapped back into the expressions and A, B
                // would be stored in the unlifts array

                val sym = Symbol.newVal(Symbol.spliceOwner, "par", tpe, Flags.EmptyFlags, Symbol.noSymbol)
                unlifts += ((task.asTerm, sym, tpe))
                Ref(sym)

                // val (sym, symId) =
                //   useNewSymbolIn(tpe)(symId => symId)
                // unlifts += ((task.asTerm, sym, tpe))
                // symId
            }

          unlifts.toList match {
            case List() =>
              println("=========== No Unlifts ==========")
              None

            /*
            For a expression (in a single block-line) that has one await in the middle of things e.g.
            { await(foo) + bar }
            Needs to turn into something like:
            { await(foo).map(fooVal => fooVal + bar) }
            When thinking about types, it looks something like:
            { (await(foo:Task[t]):t + bar):r }
            { await(foo:t):Task[t].map[r](fooVal:t => (fooVal + bar):r) }
            */
            case List((monad, name, tpe)) =>
              val out =
              (tpe.asType, newTree.tpe.asType) match
                case ('[t], '[r]) =>
                  val mtpe = MethodType(List("sm"))(_ => List(TypeRepr.of[t]), _ => TypeRepr.of[r])
                  val lam =
                    Lambda(Symbol.spliceOwner, mtpe, {
                        case (methSym, List(sm: Term)) =>
                          replaceSymbolIn(newTree)(name, sm).changeOwner(methSym)
                        case _ =>
                          report.errorAndAbort(s"Invalid lambda created for: ${Format.Tree(monad)}.flatMap of ${Format.Tree(newTree)}. This should not be possible.")
                      }
                    )
                  Some(IR.Monad('{ ${monad.asExprOf[ZIO[?, ?, t]]}.map[r](${lam.asExprOf[t => r]}) }.asTerm))

              println("=========== Single unlift: ==========\n" + Format.Term(out.get.code))
              out

            case unlifts =>
              val (terms, names, types) = unlifts.unzip3
              val termsExpr = Expr.ofList(terms.map(_.asExprOf[ZIO[?, ?, ?]]))
              val collect = '{ ZIO.collectAll(Chunk.from($termsExpr)) }
              def makeVariables(iterator: Expr[Iterator[?]]) =
                unlifts.map((monad, symbol, tpe) =>
                    tpe.asType match {
                      case '[t] =>
                        ValDef(symbol, Some('{ $iterator.next().asInstanceOf[t] }.asTerm))
                    }
                )

              val totalType = ComputeTotalZioType.valueOf(terms: _*)
              val output =
                totalType.asType match
                  case '[t] =>
                    '{
                      $collect.map(terms => {
                        val iter = terms.iterator
                        ${ Block(makeVariables('iter), newTree).asExpr }
                      }).asInstanceOf[ZIO[?, ?, t]]
                    }

              val out = Some(IR.Monad(output.asTerm))
              println(s"============ Computed Output: ${Format.TypeRepr(output.asTerm.tpe)}")
              println("=========== Multiple unlift: ==========\n" + Format.Expr(output))
              out
          }
      }
      println("================== DONE UNAPPLY ==================")
      ret
    }
  }

  // private object Nest {
  //   enum NestType:
  //     case ValDef(symbol: Symbol)
  //     case Wildcard

  //   /**
  //     * This will actually take some block of code (that is either in a ZIO or just 'pure' code)
  //     * and will nest it into the previously-sequenced ZIO. If it is ZIO code, it will
  //     * flatMap from the previously-sequenced ZIO, otherwise it map.
  //     */
  //   def apply(monad: Expr[ZIO[?, ?, ?]], nestType: NestType, bodyRaw: Expr[_]): Expr[ZIO[?, ?, ?]] = {
  //     def symbolType =
  //       nestType match
  //         case NestType.ValDef(oldSymbol) =>
  //           oldSymbol.termRef.widenTermRefByName.asType
  //         case _ =>
  //           monad.asTerm.tpe.asType match
  //             case '[ZIO[r, e, a]] => Type.of[a]

  //     // def decideMonadType[MonadType: Type] =
  //     //   val monadType = Type.of[MonadType]
  //     //   monadType match
  //     //     case '[Any] =>
  //     //       oldSymbolType match
  //     //         case Some(value) => (value.asType, true)
  //     //         case None => (monadType, false)
  //     //     case _ =>
  //     //       (monadType, false)

  //     def replaceSymbolInBody(body: Term)(newSymbolTerm: Term) =
  //       nestType match
  //         case NestType.ValDef(oldSymbol) =>
  //           /**
  //            * In a case where we have:
  //            *  val a = unlift(foobar)
  //            *  otherStuff
  //            *
  //            * We can either lift that into:
  //            *  unlift(foobar).flatMap { v => (otherStuff /*with replaced a -> v*/) }
  //            *
  //            * Or we can just do
  //            *  unlift(foobar).flatMap { v => { val a = v; otherStuff /* with the original a variable*/ } }
  //            *
  //            * I think the 2nd variant more performant but keeping 1st one (Trees.replaceIdent(...)) around for now.
  //            */
  //           //Trees.replaceIdent(using transformerQuotes)(body)(oldSymbol, newSymbolTerm.symbol)

  //           val out = replaceSymbolIn(body)(oldSymbol, newSymbolTerm)
  //           println(s"============+ Creating $oldSymbol:${Printer.TypeReprShortCode.show(oldSymbol.termRef.widen)} -> ${newSymbolTerm.show}:${Printer.TypeReprShortCode.show(newSymbolTerm.tpe.widen)} replacement let:\n${Format(Printer.TreeShortCode.show(out))}")
  //           out

  //         case NestType.Wildcard =>
  //           body

  //     bodyRaw match {
  //       // q"${Resolve.flatMap(monad.pos, monad)}(${toVal(name)} => $body)"
  //       case Transform(body) =>
  //         symbolType match
  //           case '[t] =>
  //             '{ $monad.asInstanceOf[ZIO[?, ?, t]].flatMap((v: t) =>
  //               ${replaceSymbolInBody(body)(('v).asTerm).asExprOf[ZIO[?, ?, ?]]}
  //             ).asInstanceOf[ZIO[?, ?, ?]] }

  //       // q"${Resolve.map(monad.pos, monad)}(${toVal(name)} => $body)"
  //       case body            =>
  //         symbolType match
  //           // TODO check that 'a' is the same as 't' here?
  //           case '[t] =>
  //             '{ $monad.asInstanceOf[ZIO[?, ?, t]].map((v: t) =>
  //               ${replaceSymbolInBody(body.asTerm)(('v).asTerm).asExpr}
  //             ).asInstanceOf[ZIO[?, ?, ?]] } // since the body has it's own term not specifying that here
  //     }
  //   }
  // }

  /**
    * Transform a sequence of steps
    * a; b = unlift(zio); c
    * Into a.flatMap()
    */
  private object TransformBlock {
    def unapply(block: Block): Option[IR.Monadic] =
      val Block(head, tail) = block
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

        case ValDefStatement(symbol , Transform(monad)) :: tail =>
          val out =
            BlockN(tail) match
              case Transform(monadBody) =>
                IR.FlatMap(monad, symbol, monadBody)
              case pureBody =>
                IR.Map(monad, symbol, IR.Pure(pureBody))
          Some(out)

        // TODO Validate this?
        //case MatchValDef(name, body) :: tail =>
        //  report.throwError(s"===== validef match bad body: ${body.show}")

        // other statements possible including ClassDef etc... should look into that

        case Transform(monad) :: tail =>
          //println(s"============= Block - Head Transform: ${Printer.TreeShortCode.show(monad.asTerm)} ===== ${tail.map(Format.Tree(_))}")
          tail match {
            // In this case where is one statement in the block which my definition
            // needs to have the same type as the output: e.g.
            //   val v: T = { unlift(doSomething:ZIO[_, _, T]) }
            // since we've pulled out the `doSomething` inside the signature
            // will be ZIO[_, _, T] instead of T.
            case Nil =>
              //println(s"============= Block - With zero terms: ${monad.show}")
              Some(monad)
            case list =>
              // In this case there are multiple instructions inside the seauence e.g:
              //   val v: T = { unlift(x), y /*pure*/, unlift(z:ZIO[_, _, T]) }
              // We recurse by flatMapping x, y, and unlift... but eventually the type
              // also has to be ZIO[_, _, T] since we are essentially doing:
              // x.flatMap(.. -> {y; z: ZIO[_, _, T]}). Of course the value will be ZIO[_, _, T]
              // since the last value of a nested flatMap chain is just the last instruction
              // in the nested sequence.
              //println(s"============= Block - With multiple terms: ${monad.show}, ${list.map(_.show)}")
              val out =
                BlockN(tail) match
                  case Transform(bodyMonad) =>
                    IR.FlatMap(monad, None, bodyMonad)
                  case bodyPure =>
                    IR.Map(monad, None, IR.Pure(bodyPure))
              Some(out)
          }

        // This is the recursive case of TransformBlock, it will work across multiple things
        // between blocks due to the recursion e.g:
        //   val blah = new Blah(2) // 1st part, will recurse 1st time (here)
        //   import blah._          // 2nd part, will recurse 2nd time (here)
        //   val b = unlift(ZIO.succeed(value).asInstanceOf[Task[Int]]) // Then will match valdef case
        case head :: BlockN(TransformBlock(parts)) =>
          //println(s"============= Block - With end parts: ${Format.Expr(parts)}")
          Some(IR.Block(head, parts))

        case other =>
          println(s"============= Block - None: ${other.map(Format.Tree(_))}")
          None
      }
  }


  private object TransformCases {
    // private sealed trait AppliedTree { def tree: CaseDef }
    // private case object AppliedTree {
    //   case class HasTransform(tree: CaseDef) extends AppliedTree
    //   case class NoTransform(tree: CaseDef) extends AppliedTree
    // }

    def apply(cases: List[CaseDef]): List[IR.Match.CaseDef] =
      applyMark(cases)

    private def applyMark(cases: List[CaseDef]) =
      cases.map {
        case CaseDef(pattern, cond, Transform(body)) =>
          IR.Match.CaseDef(pattern, cond, body)
        case CaseDef(pattern, cond, body) =>
          IR.Match.CaseDef(pattern, cond, IR.Monad('{ ZIO.attempt(${body.asExpr}) }.asTerm))
      }

    def unapply(cases: List[CaseDef]) =
      // If at least one of the match-cases need to be transformed, transform all of them
      Some(applyMark(cases))
  }

  def apply[T: Type](valueRaw: Expr[T]): Expr[ZIO[?, ?, ?]] = {
    import quotes.reflect._
    // val value = valueRaw.asTerm.underlyingArgument
    // // Do a top-level transform to check that there are no invalid constructs
    // Allowed.validateBlocksIn(value.asExpr)
    // // Do the main transformation
    // val transformed = Transform(value.asExpr)
    // // TODO verify that there are no await calls left. Otherwise throw an error
    // transformed
    ???
  }
}