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
import zio.asyncawait.core.metaprog.ModelPrinting
import zio.asyncawait.core.metaprog.Embedder._

// TODO replace all instances of ZIO.succeed with ZIO.attempt?
//      need to look through cases to see which ones expect errors
class Transformer(inputQuotes: Quotes) extends ModelPrinting {
  implicit val transformerQuotes = inputQuotes
  import quotes.reflect._

  private case class ZioType(r: TypeRepr, e: TypeRepr, a: TypeRepr) {
    def show = s"ZioType(${Format.TypeRepr(r)}, ${Format.TypeRepr(e)}, ${Format.TypeRepr(a)})"

    def toZioType: TypeRepr =
      (r.asType, e.asType, a.asType) match
        case ('[r], '[e], '[a]) =>
          TypeRepr.of[ZIO[r, e, a]]

    def flatMappedWith(other: ZioType) =
      ZioType(ZioType.and(r, other.r), ZioType.or(e, other.e), other.a)

    def mappedWith(other: Term) =
      ZioType(r, e, other.tpe)
  }
  private object ZioType {
    def fromZIO(zio: Term) =
      zio.tpe.asType match
        case '[ZIO[r, e, a]] =>
          ZioType(TypeRepr.of[r], TypeRepr.of[e], TypeRepr.of[a])
        case _ =>
          report.errorAndAbort(s"The type of ${Format.Term(zio)} is not a ZIO. It is: ${Format.TypeRepr(zio.tpe)}")

    // In this case the error is considered to be Nothing (since we are not wrapping error handling for pure values)
    // and the environment type is considered to be Any (it will be removed via later `ZioType.union` calls if it can be specialized).
    // Only the output type is used
    def fromPure(term: Term) =
      ZioType(TypeRepr.of[Any], TypeRepr.of[Nothing], term.tpe)

    def composeN(zioTypes: List[ZioType]): ZioType =
      val (rs, es, as) = zioTypes.map(zt => (zt.r, zt.e, zt.a)).unzip3
      val out = ZioType(andN(rs), orN(es), andN(as))
      //println(s"composeN Inputs: ${zioTypes.map(_.show)}. Output: ${out.show}")
      out

    def andN(types: List[TypeRepr]) =
      if (types.length > 0)
        types.reduce(and(_, _))
      else
        TypeRepr.of[Any]

    def orN(types: List[TypeRepr]) =
      if (types.length > 0)
        types.reduce(or(_, _))
      else
        TypeRepr.of[Nothing]

    def compose(a: ZioType, b: ZioType): ZioType =
      ZioType(and(a.r, b.r), or(a.e, b.e), and(a.a, b.a))

    def or(a: TypeRepr, b: TypeRepr) =
      (a.widen.asType, b.widen.asType) match
        case ('[at], '[bt]) =>
          TypeRepr.of[at | bt].simplified

    def and(a: TypeRepr, b: TypeRepr) =
      // if either type is Any, specialize to the thing that is narrower
      val out =
      (a.widen.asType, b.widen.asType) match
        case ('[at], '[bt]) =>
          if (a =:= TypeRepr.of[Any] && b =:= TypeRepr.of[Any])
            TypeRepr.of[Any]
          else if (a =:= TypeRepr.of[Any])
            TypeRepr.of[bt]
          else if (b =:= TypeRepr.of[Any])
            TypeRepr.of[at]
          else
           TypeRepr.of[at with bt]
      //println(s"<<<<<<<<<<<< Union of (${Format.TypeRepr(a.widen)}, ${Format.TypeRepr(b.widen)}) = ${Format.TypeRepr(out)}")
      out
  }
  private object ComputeType {
    def apply(ir: IR): ZioType =
      ir match
        case IR.Pure(code) =>
          ZioType.fromPure(code)

        case IR.FlatMap(monad, valSymbol, body) =>
          apply(monad).flatMappedWith(apply(body))

        case IR.Map(monad, valSymbol, IR.Pure(term)) =>
          apply(monad).mappedWith(term)

        case IR.Monad(code) => ZioType.fromZIO(code)

        case IR.Block(head, tail) =>
          apply(tail)

        case IR.Match(scrutinee, caseDefs) =>
          // Ultimately the scrutinee will be used if it is pure or lifted, either way we can
          // treat it as a value that will be flatMapped (or Mapped) against the caseDef values.
          val scrutineeType = apply(scrutinee)
          val caseDefTypes = caseDefs.map(caseDef => apply(caseDef.rhs))
          val caseDefTotalType = ZioType.composeN(caseDefTypes)
          scrutineeType.flatMappedWith(caseDefTotalType)

        case IR.If(cond, ifTrue, ifFalse) =>
          val condType = apply(cond)
          val expressionType = ZioType.compose(apply(ifTrue), apply(ifFalse))
          condType.flatMappedWith(expressionType)

        case IR.Bool.And(left, right) =>
          ZioType.compose(apply(left), apply(right))

        case IR.Bool.Or(left, right) =>
          ZioType.compose(apply(left), apply(right))

        case IR.Parallel(monads, body) =>
          val monadsType = ZioType.composeN(monads.map((term, _) => ZioType.fromZIO(term)))
          val bodyType = ZioType.fromPure(body)
          monadsType.flatMappedWith(bodyType)

        case IR.Bool.Pure(code) =>
          ZioType.fromPure(code)

  }

  private object Render {
    private def computeSymbolType(valSymbol: Option[Symbol], alternativeSource: Term) =
      valSymbol match
        case Some(oldSymbol) =>
          oldSymbol.termRef.widenTermRefByName.asType
        case None =>
          alternativeSource.tpe.asType match
            case '[ZIO[r, e, a]] => Type.of[a]

    private def compressBlock(accum: List[Statement] = List(), block: IR.Block): (List[Statement], Term) =
      block.tail match
        case nextBlock: IR.Block =>
          compressBlock(block.head +: accum, nextBlock)
        case otherMonad =>
          (accum, apply(otherMonad).asTerm)

    def apply(ir: IR): Expr[ZIO[?, ?, ?]] = {
      ir match
        case IR.Pure(code) => ZioApply(code)

        case IR.FlatMap(monad, valSymbol, body) => {
          val monadExpr = apply(monad)
          val bodyExpr = apply(body)
          def symbolType = computeSymbolType(valSymbol, monadExpr.asTerm)
          // Symbol type needs to be the same as the A-parameter of the ZIO, if not it's an error
          // should possibly introduce an asserition for that
          // Also:
          // TODO synthesize + eta-expand the lambda manually so it's ame is based on the previous symbol name
          symbolType match
            case '[t] =>
              '{ $monadExpr.asInstanceOf[ZIO[?, ?, t]].flatMap((v: t) =>
                ${
                  replaceSymbolInBodyMaybe(using transformerQuotes)(bodyExpr.asTerm)(valSymbol, ('v).asTerm).asExprOf[ZIO[?, ?, ?]]
                }
              ) } //.asInstanceOf[ZIO[?, ?, ?]]
        }

        // Pull out the value from IR.Pure and use it directly in the mapping
        case IR.Map(monad, valSymbol, IR.Pure(body)) =>
          val monadExpr = apply(monad)
          def symbolType = computeSymbolType(valSymbol, monadExpr.asTerm)
          symbolType match
          // TODO check that 'a' is the same as 't' here?
          case '[t] =>
            '{ $monadExpr.asInstanceOf[ZIO[?, ?, t]].map((v: t) =>
              ${replaceSymbolInBodyMaybe(using transformerQuotes)(body)(valSymbol, ('v).asTerm).asExpr}
            ) } // .asInstanceOf[ZIO[?, ?, ?]] // since the body has it's own term not specifying that here
            // TODO any ownership changes needed in the body?

        case IR.Monad(code) => code.asExprOf[ZIO[?, ?, ?]]

        case block: IR.Block =>
          val (stmts, term) = compressBlock(List(), block)
          Block(stmts, term).asExprOf[ZIO[?, ?, ?]]

        case IR.Match(scrutinee, caseDefs) =>
          scrutinee match
            case value: IR.Monadic =>
              val monadExpr = apply(value)
              // use the symbol instead of the monad as the scrutinee because the monad needs to be flatmapped first
              // (Note don't think you need to change ownership of caseDef.rhs to the new symbol but possible.)
              val matchSymbol = Symbol.newVal(Symbol.spliceOwner, "matchVar", monadExpr.asTerm.tpe, Flags.EmptyFlags, Symbol.noSymbol)
              val newCaseDefs =
                caseDefs.map { caseDef =>
                  val bodyExpr = apply(caseDef.rhs)
                  CaseDef(caseDef.pattern, caseDef.guard, bodyExpr.asTerm)
                }
              // even if the content of the match is pure we lifted it into a monad. If we want to optimize we
              // change the IR.CaseDef.rhs to be IR.Pure as well as IR.Monadic and handle both cases
              val newMatch = Match(Ref(matchSymbol), newCaseDefs)
              // We can synthesize the monadExpr.flatMap call from the monad at this point but I would rather pass it to the FlatMap case to take care of
              apply(IR.FlatMap(IR.Monad(monadExpr.asTerm), matchSymbol, IR.Monad(newMatch)))
            case IR.Pure(termValue) =>
              val newCaseDefs =
                caseDefs.map { caseDef =>
                  val bodyExpr = apply(caseDef.rhs)
                  CaseDef(caseDef.pattern, caseDef.guard, bodyExpr.asTerm)
                }
              val newMatch = Match(termValue, newCaseDefs)
              // recall that the expressions in the case defs need all be ZIO instances (i.e. monadic) we we can
              // treat the whole thing as a ZIO (i.e. monadic) expression
              newMatch.asExprOf[ZIO[?, ?, ?]]

        case IR.If(cond, ifTrue, ifFalse) =>
          enum ConditionState:
            case BothPure(ifTrue: Term, ifFalse: Term)
            case BothMonadic(ifTrue: IR.Monadic, ifFalse: IR.Monadic)

          val conditionState =
            (ifTrue, ifFalse) match {
              case (IR.Puric(a), IR.Puric(b)) => ConditionState.BothPure(a, b)
              case (IR.Puric(a), b: IR.Monadic) => ConditionState.BothMonadic(IR.Monad(ZioApply(a).asTerm), b)
              case (a: IR.Monadic, IR.Puric(b)) => ConditionState.BothMonadic(a, IR.Monad(ZioApply(b).asTerm))
              case (a: IR.Monadic, b: IR.Monadic) => ConditionState.BothMonadic(a, b)
            }

          cond match {
            case m: IR.Monadic => {
              val sym = Symbol.newVal(Symbol.spliceOwner, "ifVar", TypeRepr.of[Boolean], Flags.EmptyFlags, Symbol.noSymbol)
              conditionState match {
                // For example: if(await(something)) await(foo) else await(bar)
                // => something.map(ifVar => (foo, bar) /*replace-to-ifVar*/)
                // Note that in this case we embed foo, bar into the if-statement. They are ZIO-values which is why we need a flatMap
                case ConditionState.BothMonadic(ifTrue, ifFalse) =>
                  val ifTrueTerm = apply(ifTrue).asTerm
                  val ifFalseTerm = apply(ifFalse).asTerm
                  apply(IR.FlatMap(m, sym, IR.Monad(If(Ref(sym), ifTrueTerm, ifFalseTerm))))
                // For example: if(await(something)) "foo" else "bar"
                case ConditionState.BothPure(ifTrue, ifFalse) =>
                  apply(IR.Map(m, sym, IR.Pure(If(Ref(sym), ifTrue, ifFalse))))
              }
            }
            case IR.Bool.Pure(value) => {
              conditionState match {
                case ConditionState.BothMonadic(ifTrue, ifFalse) =>
                    val ifTrueTerm = apply(ifTrue).asTerm
                    val ifFalseTerm = apply(ifFalse).asTerm
                    ZioApply(If(value, ifTrueTerm, ifFalseTerm))
                case ConditionState.BothPure(ifTrue, ifFalse) =>
                  val ifStatement = If(value, ifTrue, ifFalse)
                  ZioApply(ifStatement)
              }
            }
          }

        case expr @ IR.Bool.And(left, right) =>
          (left, right) match {
            case (a: IR.Monadic, b: IR.Monadic) =>
              '{ ${apply(a)}.flatMap { case true => ${apply(b)}; case false => ${ZioApply.False} } }
            case (a: IR.Monadic, IR.Pure(b)) =>
              '{ ${apply(a)}.map { case true => ${b.asExpr}; case false => ${ZioApply.False} } }
            case (IR.Pure(a), b: IR.Monadic) =>
              '{ if (${a.asExprOf[Boolean]}) ${apply(b)} else ZIO.succeed(${ZioApply.False}) }
            // case Pure/Pure is taken care by in the transformer on a higher-level via the PureTree case. Still, handle them just in case
            case (IR.Bool.Pure(a), IR.Bool.Pure(b)) =>
              '{ ZIO.succeed(${a.asExprOf[Boolean]} && ${b.asExprOf[Boolean]}) }
            case _ => report.errorAndAbort(s"Invalid boolean variable combination:\n${mprint(expr)}")
          }

        case expr @ IR.Bool.Or(left, right) =>
          (left, right) match {
            case (a: IR.Monadic, b: IR.Monadic) =>
              '{ ${apply(a)}.flatMap { case true => ${ZioApply.True}; case false => ${apply(b)}  } }
            case (a: IR.Monadic, IR.Pure(b)) =>
              '{ ${apply(a)}.map { case true => ${ZioApply.True}; case false => ${b.asExpr} } }
            case (IR.Pure(a), b: IR.Monadic) =>
              '{ if (${a.asExprOf[Boolean]}) ${ZioApply.True} else ${apply(b)} }
            // case Pure/Pure is taken care by in the transformer on a higher-level via the PureTree case. Still, handle them just in case
            case (IR.Bool.Pure(a), IR.Bool.Pure(b)) =>
              '{ ZIO.succeed(${a.asExprOf[Boolean]} || ${b.asExprOf[Boolean]}) }
            case _ => report.errorAndAbort(s"Invalid boolean variable combination:\n${mprint(expr)}")
          }

        // Note, does not get here when it's a IR.Map function, the value is directly embedded since it
        // does not need to be wrapped into a ZIO in that case.
        case IR.Bool.Pure(code) =>
          '{ ZIO.succeed(${code.asExpr}) }

        case IR.Parallel(unlifts, newTree) =>
          unlifts.toList match {
            case List() =>
              println("=========== No Unlifts ==========")
              '{ ZIO.succeed(${newTree.asExpr}) }

            /*
            For a expression (in a single block-line) that has one await in the middle of things e.g.
            { await(foo) + bar }
            Needs to turn into something like:
            { await(foo).map(fooVal => fooVal + bar) }
            When thinking about types, it looks something like:
            { (await(foo:Task[t]):t + bar):r }
            { await(foo:t):Task[t].map[r](fooVal:t => (fooVal + bar):r) }
            */
            case List((monad, name)) =>
              val out =
              (monad.tpe.asType, newTree.tpe.asType) match
                case ('[ZIO[x, y, t]], '[r]) =>
                  val mtpe = MethodType(List("sm"))(_ => List(TypeRepr.of[t]), _ => TypeRepr.of[r])
                  val lam =
                    Lambda(Symbol.spliceOwner, mtpe, {
                        case (methSym, List(sm: Term)) =>
                          replaceSymbolIn(newTree)(name, sm).changeOwner(methSym)
                        case _ =>
                          report.errorAndAbort(s"Invalid lambda created for: ${Format.Tree(monad)}.flatMap of ${Format.Tree(newTree)}. This should not be possible.")
                      }
                    )
                  apply(IR.Monad('{ ${monad.asExprOf[ZIO[?, ?, t]]}.map[r](${lam.asExprOf[t => r]}) }.asTerm))
              //println("=========== Single unlift: ==========\n" + Format.Term(out.get.code))
              out

            case unlifts =>
              val unliftTriples =
                unlifts.map(
                  (term, name) => {
                    val tpe =
                      term.tpe.asType match
                        case '[ZIO[x, y, t]] => TypeRepr.of[t]
                    (term, name, tpe)
                })
              val (terms, names, types) = unliftTriples.unzip3
              val termsExpr = Expr.ofList(terms.map(_.asExprOf[ZIO[?, ?, ?]]))
              val collect = '{ ZIO.collectAll(Chunk.from($termsExpr)) }
              def makeVariables(iterator: Expr[Iterator[?]]) =
                unliftTriples.map((monad, symbol, tpe) =>
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

              val out = apply(IR.Monad(output.asTerm))
              // println(s"============ Computed Output: ${Format.TypeRepr(output.asTerm.tpe)}")
              // println("=========== Multiple unlift: ==========\n" + Format.Expr(output))
              out
          }
    }
  }

  private object Transform {
    def apply(term: Term): IR.Monadic =
      unapply(term).getOrElse(IR.Monad(ZioApply(term).asTerm))

    // TODO really use underlyingArgument????
    def unapply(expr: Term): Option[IR.Monadic] = {
      val ret = expr match {
        case PureTree(tree) =>
          None

        case If(cond, ifTrue, ifFalse) =>
          // NOTE: Code below is quite inefficient, do instead do this:
          // (Transform.unapply(ifTrue), Transform.unapply(ifFalse)). Then match on the Some/Some, Some/None, etc... cases
          val (ifTrueIR, ifFalseIR) =
            (ifTrue, ifFalse) match {
              case (Transform(ifTrue), Transform(ifFalse)) =>
                (ifTrue, ifFalse)
              case (Transform(ifTrue), ifFalse) =>
                (ifTrue, IR.Pure(ifFalse))
              case (ifTrue, Transform(ifFalse)) =>
                (IR.Pure(ifTrue), ifFalse)
              case (ifTrue, ifFalse) =>
                (IR.Pure(ifTrue), IR.Pure(ifFalse))
          }
          val condIR: IR.Monadic | IR.Bool.Pure =
            cond match
              case Transform(monad) => monad
              case _ => IR.Bool.Pure(cond)
          Some(IR.If(condIR, ifTrueIR, ifFalseIR))

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


        // This is perhaps the most powerful component of the monadless-paradigm. It changes something that looks like this:
        // { (unlift(foo), unlift(bar)) }
        // To something that looks like:
        // { ZIO.collect(foo, bar).map(iter => val a = iter.next(); val b = iter.next(); (a, b)) }
        case term => // @ Allowed.ParallelExpression()
          val unlifts = mutable.ArrayBuffer.empty[(Term, Symbol)]
          val newTree: Term =
            Trees.Transform(term, Symbol.spliceOwner) {
              case Seal('{ await[r, e, a]($task) }) =>
                // TODO Maybe should store this type in the ulifts after-all to not have to compute it multiple times?
                val tpe =
                  task.asTerm.tpe.asType match
                    case '[ZIO[x, y, t]] => TypeRepr.of[t]
                val sym = Symbol.newVal(Symbol.spliceOwner, "par", tpe, Flags.EmptyFlags, Symbol.noSymbol)
                unlifts += ((task.asTerm, sym))
                Ref(sym)
            }
          Some(IR.Parallel(unlifts.toList, newTree))
      }
      //println("================== DONE UNAPPLY ==================")
      ret
    }
  }

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

  def symbolLineage(sym: Symbol): Unit =
    if (sym.isNoSymbol) {
      println(s"------- NO SYMBOL")
      ()
    } else {
      println(s"------- Symbol: ${sym}. Synthetic: ${sym.flags.is(Flags.Synthetic)}. Macro: ${sym.flags.is(Flags.Macro)}") //Flags: ${sym.flags.show}
      symbolLineage(sym.owner)
    }

  def apply[T: Type](valueRaw: Expr[T]): Expr[ZIO[?, ?, ?]] = {
    val value = valueRaw.asTerm.underlyingArgument
    // // Do a top-level transform to check that there are no invalid constructs
    // Allowed.validateBlocksIn(value.asExpr)
    // // Do the main transformation
    val transformed = Transform(value)

    println("============== Before Render ==============")
    println(mprint(transformed))

    val output = Render(transformed)
    println("============== After Render ==============")
    println(Format.Expr(output))

    val computedType = ComputeType(transformed)

    val zioType = computedType.toZioType
    println(s"-------- Computed-Type: ${Format.TypeRepr(zioType)}. Discovered-Type: ${Format.TypeRepr(output.asTerm.tpe)}. Is Subtype: ${zioType <:< output.asTerm.tpe}")

    // // TODO verify that there are no await calls left. Otherwise throw an error
    val ownerPositionOpt = topLevelOwner.pos

    (computedType.r.asType, computedType.e.asType, computedType.a.asType) match {
      case ('[r], '[e], '[a]) =>
        val computedTypeMsg = s"Computed Type: ${Format.TypeOf[ZIO[r, e, a]]}"

        ownerPositionOpt match {
          case Some(pos) =>
            report.info(computedTypeMsg, pos)
          case None =>
            report.info(computedTypeMsg)
        }
        '{ $output.asInstanceOf[ZIO[r, e, a]] }
    }

    //  '{ $output.asInstanceOf[ZIO[Any, Throwable, T]] }
  }
}