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

class Transformer(using transformerQuotes: Quotes) {
  import quotes.reflect._

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

    println(s"============  Making New Symbol For: ${symbol} -> ${Printer.TreeShortCode.show(body)}")
    (symbol, body)
  }

  object Transform {
    def apply(expr: Expr[?]): Expr[ZIO[Any, Throwable, ?]] =
      unapply(expr.asTerm.underlyingArgument.asExpr)
        .getOrElse('{ ZIO.succeed($expr) })

    // TODO really use underlyingArgument????
    def unapply(expr: Expr[?]): Option[Expr[ZIO[Any, Throwable, ?]]] = {
      println(s"================== UNAPPLY: ${Format.Expr(expr)} ==================")
      val ret = expr match {
        case Unseal(PureTree(tree)) =>
          println(s"============  Tree is Pure!: ${tree.show}")
          None

        case Unseal(If(cond, ifTrue, ifFalse)) =>
          cond.asExpr match
            case Transform(monad) =>
              val sym = Symbol.newVal(Symbol.spliceOwner, "ifVar", TypeRepr.of[Boolean], Flags.EmptyFlags, Symbol.noSymbol)
              val body = If(Ref(sym), ifTrue, ifFalse)
              Some(Nest(monad, Nest.NestType.ValDef(sym), body.asExpr))
            case _ =>
              (ifTrue.asExpr, ifFalse.asExpr) match {
                case (Transform(ifTrue), Transform(ifFalse)) =>
                  Some(If(cond, ifTrue.asTerm, ifFalse.asTerm).asExprOf[ZIO[Any, Throwable, ?]])
                case (Transform(ifTrue), ifFalse) =>
                  Some(If(cond, ifTrue.asTerm, '{ ZIO.succeed(${ifFalse}) }.asTerm).asExprOf[ZIO[Any, Throwable, ?]])
                case (ifTrue, Transform(ifFalse)) =>
                  Some(If(cond, '{ ZIO.succeed(${ifTrue}) }.asTerm, ifFalse.asTerm).asExprOf[ZIO[Any, Throwable, ?]])
                case (ifTrue, ifFalse) =>
                  None
              }

        case '{ ($a: Boolean) && ($b: Boolean) } =>
          (a, b) match {
            case (Transform(a), Transform(b)) =>
              Some('{ $a.flatMap { case true => $b; case false => ZIO.succeed(false) } })
            case (Transform(a), b) =>
              Some('{ $a.map { case true => $b; case false => false } })
            case (a, Transform(b)) =>
              Some('{ if ($a) $b else ZIO.succeed(false) })
          }

        case '{ ($a: Boolean) || ($b: Boolean) } =>
          (a, b) match {
            case (Transform(a), Transform(b)) =>
              Some('{ $a.flatMap { case true => ZIO.succeed(true); case false => $b } })
            case (Transform(a), b) =>
              Some('{ $a.map { case true => true; case false => $b } })
            case (a, Transform(b)) =>
              Some('{ if ($a) ZIO.succeed(true) else $b })
          }

        case Unseal(block @ Block(parts, lastPart)) if (parts.nonEmpty) =>
          println(s"============  Transform a Block: ${parts.map(part => Format.Tree(part)).mkString("List(\n", ",\n", ")\n")} ==== ${Format.Tree(lastPart)}")
          TransformBlock.unapply(block)

        case Unseal(Match(m @ Seal(Transform(monad)), caseDefs)) =>
          println(s"============  Body Has Monad =======\n${Printer.TreeShortCode.show(m)}")
          println(s"====== Transformed:\n" + Format.Expr(monad))
          println(s"====== Monad Tpe:\n" + Format.TypeRepr(monad.asTerm.tpe))
          println(s"====== Match Tpe:\n" + Format.TypeRepr(m.tpe))

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

          val s = oldSymbol
          println(s"========= SYMBOL INFO ${s.owner}/${Symbol.spliceOwner}, ${s.name}, ${s.flags.show}, ${s.privateWithin}")

          val out = Nest(monad, Nest.NestType.ValDef(oldSymbol), body.asExpr)
          Some(out)

          // val sym = Symbol.newVal(Symbol.spliceOwner, "m", m.tpe, Flags.EmptyFlags, Symbol.noSymbol)
          // val body = Match(Ident(sym.termRef), caseDefs)
          // val out = Nest(monad, Nest.NestType.ValDef(sym), body.asExpr)
          // println(s"============  Body Has Monad RETURN =======\n${Printer.TreeShortCode.show(out.asTerm)}")
          // Some(out)

        case Unseal(m @ Match(value, TransformCases(cases))) =>
          println(s"=============== Transform Inner Cases")
          Some(Match(value, cases).asExprOf[ZIO[Any, Throwable, ?]])

        case '{ await[t]($task) } =>
          println(s"=============== Unlift: ${task.show}")
          Some(task)

        case Unseal(Typed(tree, _)) =>
          println(s"=============== Untype: ${tree.show}")
          unapply(tree.asExpr)

        // Change something that looks like this:
        // { (unlift(foo), unlift(bar)) }
        // To something that looks like:
        // { ZIO.collect(foo, bar).map(iter => val a = iter.next(); val b = iter.next(); (a, b)) }
        // TODO MAJOR Only support parallel awaits for strict-set of known constructs
        //      Examine set of cases where this occurs. Then as a final case
        //      Check that there are awaits in any other kind of construct
        //      and throw an unsupproted-construct exception.
        case expr => // @ Allowed.ParallelExpression()
          println(s"=============== Other: ${Format(Printer.TreeShortCode.show(expr.asTerm))}")
          val unlifts = mutable.ArrayBuffer.empty[(Term, Symbol, TypeRepr)]
          val newTree: Term =
            Trees.Transform(expr.asTerm, Symbol.spliceOwner) {
              case Seal('{ await[t]($task) }) =>
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
                  Some('{ ${monad.asExprOf[ZIO[Any, Throwable, t]]}.map[r](${lam.asExprOf[t => r]}) })

              println("=========== Single unlift: ==========\n" + Format.Expr(out.get))
              out

            case unlifts =>
              val (terms, names, types) = unlifts.unzip3
              val termsExpr = Expr.ofList(terms.map(_.asExprOf[ZIO[Any, Throwable, ?]]))
              val collect = '{ ZIO.collectAll(Chunk.from($termsExpr)) }
              def makeVariables(iterator: Expr[Iterator[?]]) =
                unlifts.map((monad, symbol, tpe) =>
                    tpe.asType match {
                      case '[t] =>
                        ValDef(symbol, Some('{ $iterator.next().asInstanceOf[t] }.asTerm))
                    }
                )

              val totalType = ComputeTotalType.of(terms)


              val output =
                totalType.asType match
                  case '[t] =>
                    '{
                      $collect.map(terms => {
                        val iter = terms.iterator
                        ${ Block(makeVariables('iter), newTree).asExpr }
                      }).asInstanceOf[ZIO[Any, Throwable, t]]
                    }

              val out = Some(output)
              println(s"============ Computed Output: ${Format.TypeRepr(output.asTerm.tpe)}")
              println("=========== Multiple unlift: ==========\n" + Format.Expr(out.get))
              out
          }
      }
      println("================== DONE UNAPPLY ==================")
      ret
    }
  }

  private object ComputeTotalType {
    // Assuming it is a non-empty list
    def of(terms: List[Term]) =
      terms.drop(1).foldLeft(terms.head.tpe)((tpe, additionalTerm) =>
        tpe.asType match
          case '[ZIO[x, y, a]] =>
            additionalTerm.tpe.asType match
              case '[ZIO[x1, y1, b]] =>
                TypeRepr.of[a with b]
      )

  }

  private object Nest {
    enum NestType:
      case ValDef(symbol: Symbol)
      case Wildcard

    /**
      * This will actually take some block of code (that is either in a ZIO or just 'pure' code)
      * and will nest it into the previously-sequenced ZIO. If it is ZIO code, it will
      * flatMap from the previously-sequenced ZIO, otherwise it map.
      */
    def apply(monad: Expr[ZIO[Any, Throwable, ?]], nestType: NestType, bodyRaw: Expr[_]): Expr[ZIO[Any, Throwable, ?]] = {
      def symbolType =
        nestType match
          case NestType.ValDef(oldSymbol) =>
            oldSymbol.termRef.widenTermRefByName.asType
          case _ =>
            monad.asTerm.tpe.asType match
              case '[ZIO[Any, Throwable, t]] => Type.of[t]

      // def decideMonadType[MonadType: Type] =
      //   val monadType = Type.of[MonadType]
      //   monadType match
      //     case '[Any] =>
      //       oldSymbolType match
      //         case Some(value) => (value.asType, true)
      //         case None => (monadType, false)
      //     case _ =>
      //       (monadType, false)

      def replaceSymbolInBody(body: Term)(newSymbolTerm: Term) =
        nestType match
          case NestType.ValDef(oldSymbol) =>
            /**
             * In a case where we have:
             *  val a = unlift(foobar)
             *  otherStuff
             *
             * We can either lift that into:
             *  unlift(foobar).flatMap { v => (otherStuff /*with replaced a -> v*/) }
             *
             * Or we can just do
             *  unlift(foobar).flatMap { v => { val a = v; otherStuff /* with the original a variable*/ } }
             *
             * I think the 2nd variant more performant but keeping 1st one (Trees.replaceIdent(...)) around for now.
             */
            //Trees.replaceIdent(using transformerQuotes)(body)(oldSymbol, newSymbolTerm.symbol)

            val out = replaceSymbolIn(body)(oldSymbol, newSymbolTerm)
            println(s"============+ Creating $oldSymbol:${Printer.TypeReprShortCode.show(oldSymbol.termRef.widen)} -> ${newSymbolTerm.show}:${Printer.TypeReprShortCode.show(newSymbolTerm.tpe.widen)} replacement let:\n${Format(Printer.TreeShortCode.show(out))}")
            out

          case NestType.Wildcard =>
            body

      bodyRaw match {
        // q"${Resolve.flatMap(monad.pos, monad)}(${toVal(name)} => $body)"
        case Transform(body) =>
          println(s"=================== Flat Mapping: ${Format(Printer.TreeShortCode.show(body.asTerm))}")
          println(s"Monad Type: ${monad.asTerm.tpe.show}")
          symbolType match
            case '[t] =>
              '{ $monad.asInstanceOf[ZIO[Any, Throwable, t]].flatMap((v: t) =>
                ${replaceSymbolInBody(body.asTerm)(('v).asTerm).asExprOf[ZIO[Any, Throwable, ?]]}
              ) }

        // q"${Resolve.map(monad.pos, monad)}(${toVal(name)} => $body)"
        case body            =>
          println(s"=================== Mapping: ${Format(Printer.TreeShortCode.show(body.asTerm))}")
          symbolType match
            case '[t] =>
              '{ $monad.asInstanceOf[ZIO[Any, Throwable, t]].map((v: t) =>
                ${replaceSymbolInBody(body.asTerm)(('v).asTerm).asExpr}
              ) }
      }
    }
  }

  /**
    * Transform a sequence of steps
    * a; b = unlift(zio); c
    * Into a.flatMap()
    */
  private object TransformBlock {
    def unapply(block: Block): Option[Expr[ZIO[Any, Throwable, ?]]] =
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
        case ValDefStatement(symbol , Seal(Transform(monad))) :: tail =>
          println(s"============= Block - Val Def: ${Printer.TreeShortCode.show(monad.asTerm)}")
          val nest = Nest(monad, Nest.NestType.ValDef(symbol), BlockN(tail).asExpr)
          Some(nest.asExprOf[ZIO[Any, Throwable, ?]])

        // TODO Validate this?
        //case MatchValDef(name, body) :: tail =>
        //  report.throwError(s"===== validef match bad body: ${body.show}")

        // other statements possible including ClassDef etc... should look into that

        case IsTerm(Seal(Transform(monad))) :: tail =>
          println(s"============= Block - Head Transform: ${Printer.TreeShortCode.show(monad.asTerm)} ===== ${tail.map(Format.Tree(_))}")
          tail match {
            // In this case where is one statement in the block which my definition
            // needs to have the same type as the output: e.g.
            //   val v: T = { unlift(doSomething:ZIO[_, _, T]) }
            // since we've pulled out the `doSomething` inside the signature
            // will be ZIO[_, _, T] instead of T.
            case Nil =>
              println(s"============= Block - With zero terms: ${monad.show}")
              Some(monad.asExprOf[ZIO[Any, Throwable, ?]])
            case list =>
              // In this case there are multiple instructions inside the seauence e.g:
              //   val v: T = { unlift(x), y /*pure*/, unlift(z:ZIO[_, _, T]) }
              // We recurse by flatMapping x, y, and unlift... but eventually the type
              // also has to be ZIO[_, _, T] since we are essentially doing:
              // x.flatMap(.. -> {y; z: ZIO[_, _, T]}). Of course the value will be ZIO[_, _, T]
              // since the last value of a nested flatMap chain is just the last instruction
              // in the nested sequence.
              println(s"============= Block - With multiple terms: ${monad.show}, ${list.map(_.show)}")
              val nest = Nest(monad, Nest.NestType.Wildcard, BlockN(tail).asExpr)
              Some(nest.asExprOf[ZIO[Any, Throwable, ?]])
          }

        // This is the recursive case of TransformBlock, it will work across multiple things
        // between blocks due to the recursion e.g:
        //   val blah = new Blah(2) // 1st part, will recurse 1st time (here)
        //   import blah._          // 2nd part, will recurse 2nd time (here)
        //   val b = unlift(ZIO.succeed(value).asInstanceOf[Task[Int]]) // Then will match valdef case
        case head :: BlockN(TransformBlock(parts)) =>
          println(s"============= Block - With end parts: ${Format.Expr(parts)}")
          Some(BlockN(List(head, parts.asTerm)).asExprOf[ZIO[Any, Throwable, ?]])

        case other =>
          println(s"============= Block - None: ${other.map(Format.Tree(_))}")
          None
      }
  }


  private object TransformCases {
    private sealed trait AppliedTree { def tree: CaseDef }
    private case object AppliedTree {
      case class HasTransform(tree: CaseDef) extends AppliedTree
      case class NoTransform(tree: CaseDef) extends AppliedTree
    }

    def apply(cases: List[CaseDef]): List[CaseDef] =
      applyMark(cases).map(_.tree)

    private def applyMark(cases: List[CaseDef]) =
      cases.map {
        case CaseDef(pattern, cond, Seal(Transform(body))) => AppliedTree.HasTransform(CaseDef(pattern, cond, body.asTerm))
        case CaseDef(pattern, cond, body) => AppliedTree.NoTransform(CaseDef(pattern, cond, '{ ZIO.attempt(${body.asExpr}) }.asTerm))
      }

    def unapply(cases: List[CaseDef]) = {
        // If at least one of the match-cases need to be transformed, transform all of them
        val mappedCases = applyMark(cases)
        if (mappedCases.exists(_.isInstanceOf[AppliedTree.HasTransform]))
          Some(mappedCases.map(_.tree))
        else
          None
      }
  }

  def apply[T: Type](valueRaw: Expr[T]): Expr[ZIO[Any, Throwable, T]] = {
    import quotes.reflect._
    val value = valueRaw.asTerm.underlyingArgument
    // Do a top-level transform to check that there are no invalid constructs
    Allowed.validateBlocksIn(value.asExpr)
    // Do the main transformation
    val transformed = Transform(value.asExpr)
    // TODO verify that there are no await calls left. Otherwise throw an error
    '{ ${transformed}.asInstanceOf[ZIO[Any, Throwable, T]] }
  }
}