// package zio.direct.core.metaprog

// import zio.direct.core.util.WithUnsupported
// import zio.direct.core.util.Messages

// trait WithAllowed extends MacroBase {
//   self: WithUnsupported =>

//   import c.universe._

//   object Allowed {

//     object FromMutablePackage {
//       def check(tpe: Type) = {

//         val fullName = tpe.typeSymbol.fullName
//         (
//           fullName.startsWith("scala.collection.mutable") ||
//             tpe <:< typeOf[scala.collection.Iterator[Any]] ||
//             tpe <:< typeOf[scala.Array[Any]] ||
//             tpe <:< typeOf[java.util.Collection[Any]] ||
//             tpe <:< typeOf[java.util.Map[Any, Any]]
//         ) && (!(tpe =:= typeOf[Nothing]))
//       }
//     }

//     def finalValidtyCheck(expr: Tree, instructions: Instructions) = {
//       implicit val implicitInstr: Instructions = instructions
//       Trees.traverse(c)(expr) {
//         // If there are code blocks remaining that have arbitrary zio values it means that
//         // they will be lost because the transformations for block-stmts to map/flatMap chains are already done.
//         case Block(stmts, output) =>
//           stmts.foreach(Unsupported.Warn.checkUnmooredZio(_))
//         case tree @ RunCall(_) =>
//           Unsupported.Error.withTree(tree, Messages.RunRemainingAfterTransformer)
//       }
//     }

//     def validateBlocksIn(expr: Tree, instructions: Instructions): Unit = {
//       implicit val implicitInstr: Instructions = instructions
//       validateBlocksTree(expr)
//     }

//     private def validateRunClause(expr: Tree)(implicit instructions: Instructions): Unit =
//       Trees.traverse(c)(expr) {
//         // Cannot have nested runs:
//         case tree @ RunCall(_) =>
//           Unsupported.Error.withTree(tree, Messages.RunInRunError)

//         // Assignment in an run allowed by not recommenteded
//         case asi: Assign if (instructions.verify == Verify.Strict) =>
//           Unsupported.Warn.withTree(asi, Messages.RunAssignmentNotRecommended)
//       }

//     // TODO this way of traversing the tree is error prone not not very efficient. Re-write this
//     // using the TreeTraverser directly and make `traverse` private.
//     private def validateBlocksTree(inputTree: Tree)(implicit instructions: Instructions): Unit = {
//       val declsErrorMsg =
//         instructions.verify match {
//           case Verify.Strict  => Messages.DeclarationNotAllowed
//           case Verify.Lenient => Messages.DeclarationNotAllowedWithRuns
//           // Verify.None should stop "Allowed" from running so this should not be used
//           case Verify.None => "No Verification is being done."
//         }

//       // Unless we are on "silent model," print enough diagnostic information to know why
//       // the error is happening.
//       def declsError(intro: String, tree: Tree) =
//         if (instructions.info == InfoBehavior.Silent)
//           s"${intro}. ${declsErrorMsg}"
//         else
//           s"""|${intro}. ${declsErrorMsg}
//               |== Invalid Statement ========
//               |${if (instructions.info == InfoBehavior.VerboseTree) Format(showRaw(tree)) else Format(Format.Tree(tree))}
//               |== Invalid Block ========
//               |${if (instructions.info == InfoBehavior.VerboseTree) Format(showRaw(inputTree)) else Format(Format.Tree(inputTree))}
//               |""".stripMargin

//       sealed trait Next
//       object Next {
//         case object Proceed extends Next
//         case class ProceeedSpecific(terms: List[Tree]) extends Next
//         case object Exit extends Next
//       }

//       def validate(expr: Tree): Next = {
//         expr match {
//           // if we have transformed the tree before then we can skip validation of the contents
//           // because the whole block is treated an an effective unit
//           case q"$pack.deferred[$r, $e, $a]($effect)" =>
//             Next.Exit

//           case q"$pack.ignore[$t]($code)" =>
//             Next.Exit

//           case CaseDef(pattern, cond, output) =>
//             cond match {
//               case _ if (cond.isEmpty) => Next.Proceed
//               case PureTree(_)         => Next.Proceed
//               case nonpure =>
//                 Unsupported.Error.runUnsupported(nonpure, "Match conditionals are not allow to contain `run`. Move the `run` call out of the match-statement.")
//             }

//           // should be handled by the tree traverser but put here just in case
//           case tree @ RunCall(content) =>
//             validateRunClause(content)
//             // Do not need other validations inside the run-clause
//             Next.Exit

//           // if verification is in "Lenient mode", allow ClassDefs, DefDefs, and ValDefs so long
//           // as there are no 'run' calls inside of them
//           case PureTree(_) if (instructions.verify == Verify.Lenient) =>
//             Next.Exit

//           // evaluate standard terms i.e. that do not have any kind of special handing such as being side of run(...) calls etc...
//           case term: Tree =>
//             validateTerm(term)

//           // Do not allow declarations inside of defer blocks. Sometimes when it's a class-constructor
//           // etc... there's no Flags.Synthetic but we still need to treat the symbol as though it is synthetic
//           // because the user is not explicitly creating it.
//           case v: ClassDef if (!v.symbol.isSynthetic && !SymbolOps.isSynthetic(v.symbol)) =>
//             Unsupported.Error.withTree(v, declsError(s"Illegal ClassDef (flags: ${show(v.mods)})", v))
//           // defdefs are not allowed inside of defer blocks unless they are auto-generated by scala
//           // (scala does that when doing implicit-functions etc...)
//           case v: DefDef if (!v.symbol.isSynthetic && !SymbolOps.isSynthetic(v.symbol)) =>
//             Unsupported.Error.withTree(v, declsError(s"Function Definitions are not allowed. (flags: ${show(v.mods)})", v))
//           case v: ValDef if (v.mods.hasFlag(Flag.MUTABLE) && !v.symbol.isSynthetic && !SymbolOps.isSynthetic(v.symbol)) =>
//             Unsupported.Error.withTree(v, declsError(s"Mutable Variable Definitions are not allowed. (flags: ${show(v.mods)})", v))
//           case v: ValDef if (v.mods.hasFlag(Flag.LAZY) && !v.symbol.isSynthetic && !SymbolOps.isSynthetic(v.symbol)) =>
//             Unsupported.Error.withTree(v, declsError(s"Lazy Variable Definitions are not allowed. (flags: ${show(v.mods)})", v))
//           // do not allow implicit inside of defer clauses (if they are not parameters)
//           case v: ValDef if ((v.symbol.isImplicit) && !v.symbol.isParameter && !v.symbol.isSynthetic && !SymbolOps.isSynthetic(v.symbol)) =>
//             Unsupported.Error.withTree(v, declsError(Messages.ImplicitsNotAllowed, v))
//           // otherwise ignore, the tree traversal will continue
//           case _ =>
//             Next.Proceed
//         }
//       }

//       def validateTerm(expr: Tree): Next =
//         expr match {
//           // special error for assignment
//           case asi: Assign =>
//             Unsupported.Error.withTree(asi, Messages.AssignmentNotAllowed)

//           case _ if (FromMutablePackage.check(expr.tpe)) =>
//             Unsupported.Error.withTree(expr, Messages.MutableCollectionDetected)

//           // All the kinds of valid things a Term can be in defer blocks
//           // Originally taken from TreeMap.transformTerm in Quotes.scala
//           case v @ Ident(name) =>
//             // Not throw error for lazy things here because modules & functions also have the lazy flag. Would need
//             // to be sure to allow all of those constructs with other flags.
//             if (v.symbol.flags.is(Flags.Mutable) && !v.symbol.isSynthetic && !SymbolOps.isSynthetic(v.symbol))
//               Unsupported.Error.withTree(v, Messages.MutableAndLazyVariablesNotAllowed + s"\n=========\n(flags: ${v.symbol.flags.show})")
//             Next.Proceed
//           case Select(qualifier, name) => Next.Proceed
//           case This(qual)              => Next.Proceed
//           case Super(qual, mix)        => Next.Proceed

//           case applyNode @ Apply(fun, args) =>
//             // If we find implicit arguments in the Apply do not check them. It is very important to skip checking of implicit arguments
//             // because they do not interfere with code correctness (implicit defs are forbidden because they are defs, implicit vals
//             // are allowed because they have to be eager anyway. However, there are situations synthesized code pulugs in implicit things
//             // that have defs inside e.g. the LightTypeTag for many zio constructs). In such cases bogus messages of "Illegal DefDef (flags: Flags.Method)."
//             // will happen in user code on terms `def tag: LightTypeTag = tag0.tag` that user haven't even seen! (because they are auto-synthesized by zio code).
//             // Unfortuately this kind erratic behavior is very difficult to actually reproduce in a test because in only appears when the compiler is confused.
//             val nonImplicitArgs = ImplicitArgs.fromFunctionMarked(applyNode).filter { case (_, stat) => !stat.isImplicit }.map(_._1)
//             Next.ProceeedSpecific(fun +: nonImplicitArgs)

//           case TypeApply(fun, args)   => Next.Proceed
//           case Literal(const)         => Next.Proceed
//           case New(tpt)               => Next.Proceed
//           case Typed(expr, tpt)       => Next.Proceed
//           case Block(stats, expr)     => Next.Proceed
//           case If(cond, thenp, elsep) => Next.Proceed
//           // Anonymous functions run from things inside of Defer can have these
//           case Closure(meth, tpt) if (meth.symbol.isSynthetic) =>
//             Next.Proceed
//           case Match(selector, cases)             => Next.Proceed
//           case Return(expr, from)                 => Next.Proceed
//           case While(cond, body)                  => Next.Proceed
//           case Try(block, cases, finalizer)       => Next.Proceed
//           case Inlined(call, bindings, expansion) => Next.Proceed
//           case SummonFrom(cases)                  => Next.Proceed
//           case v: Repeated                        => Next.Proceed

//           case otherTree =>
//             Unsupported.Error.runUnsupported(otherTree)
//         }
//       end validateTerm

//       (new Traverser {
//         override def traverse(tree: Tree): Unit = {
//           val nextStep = validate(tree)
//           nextStep match {
//             // The user can tell use not to check all further elements, just some of them.
//             case Next.ProceeedSpecific(specificTerms) =>
//               specificTerms.foreach(specificTerm => traverse(specificTerm))
//             // This is the most typical, delve into the tree that the user has specified
//             case Next.Proceed => super.traverse(tree)
//             case Next.Exit    =>
//           }
//         }
//       }).traverseTree(inputTree)(Symbol.spliceOwner)
//     }
//   }

// }
