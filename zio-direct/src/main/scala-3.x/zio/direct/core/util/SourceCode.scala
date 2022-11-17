package zio.direct.core.util

import scala.quoted._
import scala.annotation.switch
import scala.util

trait SyntaxHighlight {
  def highlightKeyword(str: String): String
  def highlightTypeDef(str: String): String
  def highlightLiteral(str: String): String
  def highlightValDef(str: String): String
  def highlightOperator(str: String): String
  def highlightAnnotation(str: String): String
  def highlightString(str: String): String
  def highlightTripleQs: String
}

object SyntaxHighlight {

  def ANSI: SyntaxHighlight = new SyntaxHighlight {
    // Keep in sync with SyntaxHighlighting
    private val NoColor = Console.RESET
    private val CommentColor = Console.BLUE
    private val KeywordColor = Console.YELLOW
    private val ValDefColor = Console.CYAN
    private val LiteralColor = Console.RED
    private val StringColor = Console.GREEN
    private val TypeColor = Console.MAGENTA
    private val AnnotationColor = Console.MAGENTA

    def highlightKeyword(str: String): String = KeywordColor + str + NoColor
    def highlightTypeDef(str: String): String = TypeColor + str + NoColor
    def highlightLiteral(str: String): String = LiteralColor + str + NoColor
    def highlightValDef(str: String): String = ValDefColor + str + NoColor
    def highlightOperator(str: String): String = TypeColor + str + NoColor
    def highlightAnnotation(str: String): String = AnnotationColor + str + NoColor
    def highlightString(str: String): String = StringColor + str + NoColor
    def highlightTripleQs: String = Console.RED_B + "???" + NoColor
  }

  def plain: SyntaxHighlight = new SyntaxHighlight {
    def highlightKeyword(str: String): String = str
    def highlightTypeDef(str: String): String = str
    def highlightLiteral(str: String): String = str
    def highlightValDef(str: String): String = str
    def highlightOperator(str: String): String = str
    def highlightAnnotation(str: String): String = str
    def highlightString(str: String): String = str
    def highlightTripleQs: String = "???"
  }
}

trait ShowDetails {
  def showImplicitFunctionParams: Boolean
  def showImplicitClauses: Boolean
  def showBoundsTypes: Boolean
  def showTypeParams: Boolean
  def showAsInstanceOf: Boolean
}

object ShowDetails {
  object Compact extends ShowDetails {
    def showImplicitFunctionParams: Boolean = false
    def showImplicitClauses: Boolean = false
    def showBoundsTypes: Boolean = false
    def showTypeParams: Boolean = false
    def showAsInstanceOf: Boolean = false
  }
  object Standard extends ShowDetails {
    def showImplicitFunctionParams: Boolean = false
    def showImplicitClauses: Boolean = false
    def showBoundsTypes: Boolean = false
    def showTypeParams: Boolean = false
    def showAsInstanceOf: Boolean = true
  }
  object Verbose extends ShowDetails {
    def showImplicitFunctionParams: Boolean = true
    def showImplicitClauses: Boolean = true
    def showBoundsTypes: Boolean = true
    def showTypeParams: Boolean = true
    def showAsInstanceOf: Boolean = true
  }
}

/**
 * Printer for fully elaborated representation of the source code. Futher customized based on SourceCode in the Dotty repo.
 * In many situations if symbol.owner cannot be found the compiler will fail with a NoDenotation.owner error. This typically
 * happens when there is some kind of upstream problem which is the cause of the real issue e.g. a foo.bar call where the `bar` doesn't
 * actually exist. That means that the NoDenotation.owner error is just noise. Therefore the show___ methods
 * now return a try that catches a custom NoDenotationException which callers of this object can deal with.
 */
object SourceCode {

  def showTree(using Quotes)(tree: quotes.reflect.Tree)(showDetails: ShowDetails, syntaxHighlight: SyntaxHighlight, fullNames: Boolean): util.Try[String] =
    util.Try(new SourceCodePrinter[quotes.type](showDetails, syntaxHighlight, fullNames).printTree(tree).result())

  def showType(using Quotes)(tpe: quotes.reflect.TypeRepr)(showDetails: ShowDetails, syntaxHighlight: SyntaxHighlight, fullNames: Boolean): util.Try[String] =
    util.Try(new SourceCodePrinter[quotes.type](showDetails, syntaxHighlight, fullNames).printType(tpe)(using None).result())

  def showConstant(using Quotes)(const: quotes.reflect.Constant)(showDetails: ShowDetails, syntaxHighlight: SyntaxHighlight, fullNames: Boolean): util.Try[String] =
    util.Try(new SourceCodePrinter[quotes.type](showDetails, syntaxHighlight, fullNames).printConstant(const).result())

  def showSymbol(using Quotes)(symbol: quotes.reflect.Symbol)(syntaxHighlight: SyntaxHighlight): String =
    symbol.fullName

  def showFlags(using Quotes)(flags: quotes.reflect.Flags)(syntaxHighlight: SyntaxHighlight): String = {
    import quotes.reflect._
    val flagList = List.newBuilder[String]
    if (flags.is(Flags.Abstract)) flagList += "abstract"
    if (flags.is(Flags.Artifact)) flagList += "artifact"
    if (flags.is(Flags.Case)) flagList += "case"
    if (flags.is(Flags.CaseAccessor)) flagList += "caseAccessor"
    if (flags.is(Flags.Contravariant)) flagList += "contravariant"
    if (flags.is(Flags.Covariant)) flagList += "covariant"
    if (flags.is(Flags.Deferred)) flagList += "deferred"
    if (flags.is(Flags.Enum)) flagList += "enum"
    if (flags.is(Flags.Erased)) flagList += "erased"
    if (flags.is(Flags.Exported)) flagList += "exported"
    if (flags.is(Flags.ExtensionMethod)) flagList += "extension"
    if (flags.is(Flags.FieldAccessor)) flagList += "accessor"
    if (flags.is(Flags.Final)) flagList += "final"
    if (flags.is(Flags.HasDefault)) flagList += "hasDefault"
    if (flags.is(Flags.Implicit)) flagList += "implicit"
    if (flags.is(Flags.Infix)) flagList += "infix"
    if (flags.is(Flags.Inline)) flagList += "inline"
    if (flags.is(Flags.JavaDefined)) flagList += "javaDefined"
    if (flags.is(Flags.JavaStatic)) flagList += "static"
    if (flags.is(Flags.Lazy)) flagList += "lazy"
    if (flags.is(Flags.Local)) flagList += "local"
    if (flags.is(Flags.Macro)) flagList += "macro"
    if (flags.is(Flags.Method)) flagList += "method"
    if (flags.is(Flags.Module)) flagList += "object"
    if (flags.is(Flags.Mutable)) flagList += "mutable"
    if (flags.is(Flags.NoInits)) flagList += "noInits"
    if (flags.is(Flags.Override)) flagList += "override"
    if (flags.is(Flags.Package)) flagList += "package"
    if (flags.is(Flags.Param)) flagList += "param"
    if (flags.is(Flags.ParamAccessor)) flagList += "paramAccessor"
    if (flags.is(Flags.Private)) flagList += "private"
    if (flags.is(Flags.PrivateLocal)) flagList += "private[this]"
    if (flags.is(Flags.Protected)) flagList += "protected"
    if (flags.is(Flags.Scala2x)) flagList += "scala2x"
    if (flags.is(Flags.Sealed)) flagList += "sealed"
    if (flags.is(Flags.StableRealizable)) flagList += "stableRealizable"
    if (flags.is(Flags.Static)) flagList += "javaStatic"
    if (flags.is(Flags.Synthetic)) flagList += "synthetic"
    if (flags.is(Flags.Trait)) flagList += "trait"
    if (flags.is(Flags.Transparent)) flagList += "transparent"
    flagList.result().mkString("/*", " ", "*/")
  }

  class NoOwnerException(msg: String) extends Exception(msg)

  private class SourceCodePrinter[Q <: Quotes & Singleton](showDetails: ShowDetails, syntaxHighlight: SyntaxHighlight, fullNames: Boolean)(using val quotes: Q) {
    import syntaxHighlight._
    import quotes.reflect._

    extension (sym: Symbol)
      def ownerSafe =
        if (sym.isNoSymbol) throw new NoOwnerException("Symbol has no owner")
        else sym.owner

    object FromChars {
      import java.lang.{Character => JCharacter}

      def isOperatorPart(c: Char): Boolean = (c: @switch) match {
        case '~' | '!' | '@' | '#' | '%' |
            '^' | '*' | '+' | '-' | '<' |
            '>' | '?' | ':' | '=' | '&' |
            '|' | '/' | '\\' => true
        case c => isSpecial(c)
      }

      def isSpecial(c: Char): Boolean = {
        val chtp = JCharacter.getType(c)
        chtp == JCharacter.MATH_SYMBOL.toInt || chtp == JCharacter.OTHER_SYMBOL.toInt
      }
    }

    private[this] val sb: StringBuilder = new StringBuilder

    private[this] var indent: Int = 0
    private def indented(printIndented: => Unit): Unit = {
      indent += 1
      printIndented
      indent -= 1
    }

    private def inParens(body: => Unit): this.type = {
      this += "("
      body
      this += ")"
    }

    private def inSquare(body: => Unit): this.type = {
      this += "["
      body
      this += "]"
    }

    private def inBlock(body: => Unit): this.type = {
      this += " {"
      indented {
        this += lineBreak()
        body
      }
      this += lineBreak() += "}"
    }

    def result(): String = sb.result()

    private def lineBreak(): String = "\n" + ("  " * indent)
    private def doubleLineBreak(): String = "\n\n" + ("  " * indent)

    def printTree(tree: Tree)(using elideThis: Option[Symbol] = None): this.type = tree match {
      case PackageObject(body) =>
        printTree(body) // Print package object

      case PackageClause(Ident(name), (inner @ PackageClause(_, _)) :: Nil) if name != "<empty>" && PackageObject.unapply(inner).isEmpty =>
        // print inner package as `package outer.inner { ... }`
        printTree(inner)

      case tree @ PackageClause(name, stats) =>
        val stats1 = stats.collect {
          case stat: PackageClause                                                                           => stat
          case stat: Definition if !(stat.symbol.flags.is(Flags.Module) && stat.symbol.flags.is(Flags.Lazy)) => stat
          case stat @ (_: Import | _: Export)                                                                => stat
        }
        name match {
          case Ident("<empty>") =>
            printTrees(stats1, lineBreak())
          case _ =>
            this += "package "
            printType(name.tpe)
            inBlock(printTrees(stats1, lineBreak()))
        }

      case Import(expr, selectors) =>
        this += "import "
        printTree(expr)
        this += "."
        printSelectors(selectors)

      case Export(expr, selectors) =>
        this += "export "
        printTree(expr)
        this += "."
        printSelectors(selectors)

      case cdef @ ClassDef(name, DefDef(_, paramss, _, _), parents, self, stats) =>
        printDefAnnotations(cdef)

        val flags = cdef.symbol.flags
        if (flags.is(Flags.Implicit)) this += highlightKeyword("implicit ")
        if (flags.is(Flags.Sealed)) this += highlightKeyword("sealed ")
        if (flags.is(Flags.Final) && !flags.is(Flags.Module)) this += highlightKeyword("final ")
        if (flags.is(Flags.Case)) this += highlightKeyword("case ")

        if (name == "package$") {
          this += highlightKeyword("package object ") += highlightTypeDef(cdef.symbol.ownerSafe.name.stripSuffix("$"))
        } else if (flags.is(Flags.Module)) this += highlightKeyword("object ") += highlightTypeDef(name.stripSuffix("$"))
        else if (flags.is(Flags.Trait)) this += highlightKeyword("trait ") += highlightTypeDef(name)
        else if (flags.is(Flags.Abstract)) this += highlightKeyword("abstract class ") += highlightTypeDef(name)
        else this += highlightKeyword("class ") += highlightTypeDef(name)

        if (!flags.is(Flags.Module)) {
          for paramClause <- paramss do
            paramClause match
              case clause @ TermParamClause(params) =>
                printArgsDefs(params)
              case TypeParamClause(params) =>
                printTargsDefs(stats.collect { case targ: TypeDef => targ }.filter(_.symbol.isTypeParam).zip(params))
        }

        val parents1 = parents.filter {
          case Apply(Select(New(tpt), _), _)                                => tpt.tpe.typeSymbol != Symbol.requiredClass("java.lang.Object")
          case TypeSelect(Select(Ident("_root_"), "scala"), "Product")      => false
          case TypeSelect(Select(Ident("_root_"), "scala"), "Serializable") => false
          case _                                                            => true
        }
        if (parents1.nonEmpty)
          this += highlightKeyword(" extends ")

        def printParent(parent: Tree /* Term | TypeTree */, needEmptyParens: Boolean = false): Unit = parent match {
          case parent: TypeTree =>
            printTypeTree(parent)(using Some(cdef.symbol))
          case TypeApply(fun, targs) =>
            printParent(fun)
          case Apply(fun @ Apply(_, _), args) =>
            printParent(fun, true)
            if (!args.isEmpty || needEmptyParens)
              inParens(printTrees(args, ", ")(using Some(cdef.symbol)))
          case Apply(fun, args) =>
            printParent(fun)
            if (!args.isEmpty || needEmptyParens)
              inParens(printTrees(args, ", ")(using Some(cdef.symbol)))
          case Select(newTree: New, _) =>
            printType(newTree.tpe)(using Some(cdef.symbol))
          case parent: Term =>
            throw new MatchError(parent.show(using Printer.TreeStructure))
        }

        def printSeparated(list: List[Tree /* Term | TypeTree */ ]): Unit = list match {
          case Nil      =>
          case x :: Nil => printParent(x)
          case x :: xs =>
            printParent(x)
            this += highlightKeyword(" with ")
            printSeparated(xs)
        }
        printSeparated(parents1)

        def keepDefinition(d: Definition): Boolean = {
          val flags = d.symbol.flags
          def isUndecompilableCaseClassMethod: Boolean = {
            // Currently the compiler does not allow overriding some of the methods generated for case classes
            d.symbol.flags.is(Flags.Synthetic) &&
            (d match {
              case DefDef("apply" | "unapply" | "writeReplace", _, _, _) if d.symbol.ownerSafe.flags.is(Flags.Module) => true
              case DefDef(n, _, _, _) if d.symbol.ownerSafe.flags.is(Flags.Case) =>
                n == "copy" ||
                  n.matches("copy\\$default\\$[1-9][0-9]*") || // default parameters for the copy method
                  n.matches("_[1-9][0-9]*") || // Getters from Product
                  n == "productElementName"
              case _ => false
            })
          }
          def isInnerModuleObject = d.symbol.flags.is(Flags.Lazy) && d.symbol.flags.is(Flags.Module)
          !flags.is(Flags.Param) && !flags.is(Flags.ParamAccessor) && !flags.is(Flags.FieldAccessor) && !isUndecompilableCaseClassMethod && !isInnerModuleObject
        }
        val stats1 = stats.collect {
          case stat: Definition if keepDefinition(stat) => stat
          case stat @ (_: Import | _: Export)           => stat
          case stat: Term                               => stat
        }

        def printBody(printSelf: Boolean) = {
          this += " {"
          indented {
            if (printSelf) {
              val Some(ValDef(name, tpt, _)) = self: @unchecked
              indented {
                val name1 = if (name == "_") "this" else name
                this += " " += highlightValDef(name1) += ": "
                printTypeTree(tpt)(using Some(cdef.symbol))
                this += " =>"
              }
            }
            this += lineBreak()
            printTrees(stats1, lineBreak())
          }
          this += lineBreak() += "}"
        }
        self match {
          case Some(ValDef(_, Singleton(_), _)) =>
            if (stats1.nonEmpty)
              printBody(printSelf = false)
          case Some(ValDef(_, _, _)) =>
            printBody(printSelf = true)
          case _ =>
            if (stats1.nonEmpty)
              printBody(printSelf = false)
        }
        this

      case tdef @ TypeDef(name, rhs) =>
        printDefAnnotations(tdef)
        this += highlightKeyword("type ")
        printTargDef((tdef, tdef), isMember = true)

      case vdef @ ValDef(name, tpt, rhs) =>
        printDefAnnotations(vdef)

        val flags = vdef.symbol.flags
        if (flags.is(Flags.Implicit)) this += highlightKeyword("implicit ")
        if (flags.is(Flags.Override)) this += highlightKeyword("override ")
        if (flags.is(Flags.Final) && !flags.is(Flags.Module)) this += highlightKeyword("final ")

        printProtectedOrPrivate(vdef)

        if (flags.is(Flags.Lazy)) this += highlightKeyword("lazy ")
        if (vdef.symbol.flags.is(Flags.Mutable)) this += highlightKeyword("var ")
        else this += highlightKeyword("val ")

        val name1 = splicedName(vdef.symbol).getOrElse(name)
        this += highlightValDef(name1) += ": "
        printTypeTree(tpt)
        rhs match {
          case Some(tree) =>
            this += " = "
            printTree(tree)
          case None =>
            this
        }

      case While(cond, body) =>
        (cond, body) match {
          case (Block(Block(Nil, body1) :: Nil, Block(Nil, cond1)), Literal(UnitConstant())) =>
            this += highlightKeyword("do ")
            printTree(body1) += highlightKeyword(" while ")
            inParens(printTree(cond1))
          case _ =>
            this += highlightKeyword("while ")
            inParens(printTree(cond)) += " "
            printTree(body)
        }

      case ddef @ DefDef(name, paramss, tpt, rhs) =>
        printDefAnnotations(ddef)

        val isConstructor = name == "<init>"

        val flags = ddef.symbol.flags
        if (flags.is(Flags.Implicit)) this += highlightKeyword("implicit ")
        if (flags.is(Flags.Inline)) this += highlightKeyword("inline ")
        if (flags.is(Flags.Override)) this += highlightKeyword("override ")
        if (flags.is(Flags.Final) && !flags.is(Flags.Module)) this += highlightKeyword("final ")

        printProtectedOrPrivate(ddef)

        val name1: String = if (isConstructor) "this" else splicedName(ddef.symbol).getOrElse(name)
        this += highlightKeyword("def ") += highlightValDef(name1)
        for clause <- paramss do
          clause match
            case clause @ TermParamClause(params) if (!clause.isImplicit && !clause.isGiven) =>
              if ((clause.isImplicit || clause.isGiven) && showDetails.showImplicitClauses)
                printArgsDefs(params)
            case clase @ TypeParamClause(params) => printTargsDefs(params.zip(params))
        if (!isConstructor) {
          this += ": "
          printTypeTree(tpt)
        }
        rhs match {
          case Some(tree) =>
            this += " = "
            printTree(tree)
          case None =>
        }
        this

      case Wildcard() =>
        this += "_"

      case tree: Ident =>
        splicedName(tree.symbol) match {
          case Some(name) => this += highlightTypeDef(name)
          case _          => printType(tree.tpe)
        }

      case Select(qual, name) =>
        if (name != "asInstanceOf" || showDetails.showAsInstanceOf)
          printQualTree(qual)
          if (name != "<init>" && name != "package")
            this += "." += name
          this
        else
          printQualTree(qual)

      case Literal(const) =>
        printConstant(const)

      case This(id) =>
        id match {
          case Some(name) =>
            this += name.stripSuffix("$") += "."
          case None =>
        }
        this += "this"

      case tree: New =>
        this += "new "
        printType(tree.tpe)

      case NamedArg(name, arg) =>
        this += name += " = "
        printTree(arg)

      case SpecialOp("throw", expr :: Nil) =>
        this += "throw "
        printTree(expr)

      case Apply(fn, args) if fn.symbol == Symbol.requiredMethod("scala.quoted.runtime.quote") =>
        args.head match {
          case Block(stats, expr) =>
            this += "'{"
            indented {
              this += lineBreak()
              printFlatBlock(stats, expr)
            }
            this += lineBreak() += "}"
          case _ =>
            this += "'{"
            printTree(args.head)
            this += "}"
        }

      case Apply(fn, arg :: Nil) if fn.symbol == Symbol.requiredMethod("scala.quoted.runtime.splice") =>
        this += "${"
        printTree(arg)
        this += "}"

      case Apply(fn, argsRaw) =>
        val firstParamList =
          for {
            methodSym <- if (fn.symbol.flags.is(Flags.Method)) Some(fn.symbol) else None
            firstParamList <- fn.symbol.paramSymss.find(params => params.headOption.exists(_.isValDef))
          } yield firstParamList

        // Sometimes we don't know directly from the Term whether it is implicit or not, try to get the
        // arg val-defs and see if they are marked implicit there. All of this needs to be added to the code
        // formatting logic.
        val argsJoined: List[(Term, Option[Symbol])] =
          firstParamList match {
            case Some(parmValDefs) if parmValDefs.length == argsRaw.length =>
              argsRaw.zip(parmValDefs.map(Some(_)))
            case _ =>
              argsRaw.map(arg => (arg, None))
          }

        val args =
          argsJoined.filter((arg, argValDef) => {
            val isTermImplict = arg.symbol.flags.is(Flags.Given) || arg.symbol.flags.is(Flags.Implicit)
            val isValDefImplicit = argValDef.exists(vd => vd.flags.is(Flags.Given) || vd.flags.is(Flags.Implicit))
            val isImplicit = isTermImplict || isValDefImplicit
            !isImplicit || showDetails.showImplicitClauses
          }).map(_._1)

        var argsPrefix = ""
        fn match {
          case Select(This(_), "<init>") => this += "this" // call to constructor inside a constructor
          case Select(qual, "apply") =>
            if qual.tpe.isContextFunctionType then
              argsPrefix += "using "
            if qual.tpe.isErasedFunctionType then
              argsPrefix += "erased "
            printQualTree(fn)
          case _ => printQualTree(fn)
        }
        val args1 = args match {
          case init :+ Typed(Repeated(Nil, _), _) => init // drop empty var args at the end
          case _                                  => args
        }
        if (!args.isEmpty)
          inParens {
            this += argsPrefix
            printTrees(args1, ", ")
          }
        else
          this += argsPrefix

      case TypeApply(fn, args) =>
        // if only type-params exist in the function (implicit parameters don't count), show them
        val noTermParams =
          (for {
            methodSym <- if (fn.symbol.flags.is(Flags.Method)) Some(fn.symbol) else None
            firstParamList <- fn.symbol.paramSymss.find(params => params.headOption.exists(p => p.isValDef && !p.flags.is(Flags.Implicit)))
          } yield firstParamList).isEmpty

        printQualTree(fn)
        fn match {
          case Select(New(Applied(_, _)), "<init>") =>
            // type bounds already printed in `fn`
            this
          case _ =>
            val isAsInstanceOf = fn.show.endsWith("asInstanceOf")
            val isAsInstanceOfAndShow = isAsInstanceOf && showDetails.showAsInstanceOf
            if (showDetails.showTypeParams || isAsInstanceOfAndShow || (!isAsInstanceOf && noTermParams))
              inSquare(printTrees(args, ", "))
            this
        }

      case Super(qual, idOpt) =>
        qual match {
          case This(Some(name)) => this += name += "."
          case This(None)       =>
        }
        this += "super"
        for (id <- idOpt)
          inSquare(this += id)
        this

      case Typed(term, tpt) =>
        tpt.tpe match {
          case Types.Repeated(_) =>
            printTree(term)
            term match {
              case Repeated(_, _) | Inlined(None, Nil, Repeated(_, _)) => this
              case _                                                   => this += ": " += highlightTypeDef("_*")
            }
          case _ =>
            inParens {
              printTree(term)
              this += (if (FromChars.isOperatorPart(sb.last)) " : " else ": ")
              def printTypeOrAnnots(tpe: TypeRepr): Unit = tpe match {
                case AnnotatedType(tp, annot) if tp == term.tpe =>
                  printAnnotation(annot)
                case AnnotatedType(tp, annot) =>
                  printTypeOrAnnots(tp)
                  this += " "
                  printAnnotation(annot)
                case tpe =>
                  printType(tpe)
              }
              printTypeOrAnnots(tpt.tpe)
            }
        }

      case Assign(lhs, rhs) =>
        printTree(lhs)
        this += " = "
        printTree(rhs)

      case tree @ Lambda(params, body) => // must come before `Block`
        if (!tree.tpe.isContextFunctionType || showDetails.showImplicitFunctionParams)
          inParens {
            printArgsDefs(params)
            this += (if tree.tpe.isContextFunctionType then " ?=> " else " => ")
            printTree(body)
          }
        else
          printTree(body)

      case Block(stats0, expr) =>
        val stats = stats0.filter {
          case tree: ValDef => !tree.symbol.flags.is(Flags.Module)
          case _            => true
        }
        printFlatBlock(stats, expr)

      case Inlined(_, bindings, expansion) =>
        printFlatBlock(bindings, expansion)

      case If(cond, thenp, elsep) =>
        this += highlightKeyword("if ")
        inParens(printTree(cond))
        this += " "
        printTree(thenp)
        this += highlightKeyword(" else ")
        printTree(elsep)

      case Match(selector, cases) =>
        printQualTree(selector)
        this += highlightKeyword(" match")
        inBlock(printCases(cases, lineBreak()))

      case SummonFrom(cases) =>
        this += highlightKeyword("summonFrom ")
        inBlock(printCases(cases, lineBreak()))

      case Try(body, cases, finallyOpt) =>
        this += highlightKeyword("try ")
        printTree(body)
        if (cases.nonEmpty) {
          this += highlightKeyword(" catch")
          inBlock(printCases(cases, lineBreak()))
        }
        finallyOpt match {
          case Some(t) =>
            this += highlightKeyword(" finally ")
            printTree(t)
          case None =>
            this
        }

      case Return(expr, from) =>
        this += "return "
        printTree(expr)

      case Repeated(elems, _) =>
        printTrees(elems, ", ")

      case TypeBoundsTree(lo, hi) =>
        if (showDetails.showBoundsTypes)
          this += "_ >: "
          printTypeTree(lo)
          this += " <: "
          printTypeTree(hi)
        else
          this += "_"

      case tpt: WildcardTypeTree =>
        printType(tpt.tpe)

      case tpt: TypeTree =>
        printTypeTree(tpt)

      case Closure(meth, _) =>
        printTree(meth)

      case _: TypedOrTest | _: Unapply | _: Alternatives | _: Bind =>
        printPattern(tree)

      case tree: CaseDef =>
        printCaseDef(tree)

      case _ =>
        throw new MatchError(tree.show(using Printer.TreeStructure))

    }

    private def printQualTree(tree: Tree): this.type = tree match {
      case _: If | _: Match | _: While | _: Try | _: Return =>
        this += "("
        printTree(tree)
        this += ")"
      case _ => printTree(tree)
    }

    private def flatBlock(stats: List[Statement], expr: Term): (List[Statement], Term) = {
      val flatStats = List.newBuilder[Statement]
      def extractFlatStats(stat: Statement): Unit = stat match {
        case Lambda(_, _) => // must come before `Block`
          flatStats += stat
        case Block(stats1, expr1) =>
          val it = stats1.iterator
          while (it.hasNext)
            extractFlatStats(it.next())
          extractFlatStats(expr1)
        case Inlined(_, bindings, expansion) =>
          val it = bindings.iterator
          while (it.hasNext)
            extractFlatStats(it.next())
          extractFlatStats(expansion)
        case Literal(UnitConstant()) => // ignore
        case stat                    => flatStats += stat
      }
      def extractFlatExpr(term: Term): Term = term match {
        case Lambda(_, _) => // must come before `Block`
          term
        case Block(stats1, expr1) =>
          val it = stats1.iterator
          while (it.hasNext)
            extractFlatStats(it.next())
          extractFlatExpr(expr1)
        case Inlined(_, bindings, expansion) =>
          val it = bindings.iterator
          while (it.hasNext)
            extractFlatStats(it.next())
          extractFlatExpr(expansion)
        case term => term
      }
      val it = stats.iterator
      while (it.hasNext)
        extractFlatStats(it.next())
      val flatExpr = extractFlatExpr(expr)
      (flatStats.result(), flatExpr)
    }

    private def printFlatBlock(stats: List[Statement], expr: Term)(using elideThis: Option[Symbol]): this.type = {
      val (stats1, expr1) = flatBlock(stats, expr)
      val splicedTypeAnnot = Symbol.requiredClass("scala.quoted.runtime.SplicedType").primaryConstructor
      val stats2 = stats1.filter {
        case tree: TypeDef => !tree.symbol.hasAnnotation(splicedTypeAnnot)
        case _             => true
      }
      if (stats2.isEmpty) {
        printTree(expr1)
      } else {
        this += "{"
        indented {
          printStats(stats2, expr1)
        }
        this += lineBreak() += "}"
      }
    }

    private def printStats(stats: List[Tree], expr: Tree)(using eliseThis: Option[Symbol]): Unit = {
      def printSeparator(next: Tree): Unit = {
        // Avoid accidental application of opening `{` on next line with a double break
        def rec(next: Tree): Unit = next match {
          case Lambda(_, _)                                 => this += lineBreak()
          case Block(stats, _) if stats.nonEmpty            => this += doubleLineBreak()
          case Inlined(_, bindings, _) if bindings.nonEmpty => this += doubleLineBreak()
          case Select(qual, _)                              => rec(qual)
          case Apply(fn, _)                                 => rec(fn)
          case TypeApply(fn, _)                             => rec(fn)
          case Typed(_, _)                                  => this += doubleLineBreak()
          case _                                            => this += lineBreak()
        }
        next match {
          case term: Term =>
            flatBlock(Nil, term) match {
              case (next :: _, _) => rec(next)
              case (Nil, next)    => rec(next)
            }
          case _ => this += lineBreak()
        }
      }
      def printSeparated(list: List[Tree]): Unit = list match {
        case Nil =>
          printTree(expr)
        case x :: xs =>
          printTree(x)
          printSeparator(if (xs.isEmpty) expr else xs.head)
          printSeparated(xs)
      }

      this += lineBreak()
      printSeparated(stats)
    }

    private def printList[T](xs: List[T], sep: String, print: T => this.type): this.type = {
      def printSeparated(list: List[T]): Unit = list match {
        case Nil      =>
        case x :: Nil => print(x)
        case x :: xs =>
          print(x)
          this += sep
          printSeparated(xs)
      }
      printSeparated(xs)
      this
    }

    private def printTrees(trees: List[Tree], sep: String)(using elideThis: Option[Symbol]): this.type =
      printList(trees, sep, (t: Tree) => printTree(t))

    private def printTypeTrees(trees: List[TypeTree], sep: String)(using elideThis: Option[Symbol] = None): this.type =
      printList(trees, sep, (t: TypeTree) => printTypeTree(t))

    private def printTypes(trees: List[TypeRepr], sep: String)(using elideThis: Option[Symbol]): this.type = {
      def printSeparated(list: List[TypeRepr]): Unit = list match {
        case Nil      =>
        case x :: Nil => printType(x)
        case x :: xs =>
          printType(x)
          this += sep
          printSeparated(xs)
      }
      printSeparated(trees)
      this
    }

    private def printSelectors(selectors: List[Selector]): this.type = {
      def printSeparated(list: List[Selector]): Unit = list match {
        case Nil      =>
        case x :: Nil => printSelector(x)
        case x :: xs =>
          printSelector(x)
          this += ", "
          printSeparated(xs)
      }
      this += "{"
      printSeparated(selectors)
      this += "}"
    }

    private def printCases(cases: List[CaseDef], sep: String): this.type = {
      def printSeparated(list: List[CaseDef]): Unit = list match {
        case Nil      =>
        case x :: Nil => printCaseDef(x)
        case x :: xs =>
          printCaseDef(x)
          this += sep
          printSeparated(xs)
      }
      printSeparated(cases)
      this
    }

    private def printTypeCases(cases: List[TypeCaseDef], sep: String): this.type = {
      def printSeparated(list: List[TypeCaseDef]): Unit = list match {
        case Nil      =>
        case x :: Nil => printTypeCaseDef(x)
        case x :: xs =>
          printTypeCaseDef(x)
          this += sep
          printSeparated(xs)
      }
      printSeparated(cases)
      this
    }

    private def printPatterns(cases: List[Tree], sep: String): this.type = {
      def printSeparated(list: List[Tree]): Unit = list match {
        case Nil      =>
        case x :: Nil => printPattern(x)
        case x :: xs =>
          printPattern(x)
          this += sep
          printSeparated(xs)
      }
      printSeparated(cases)
      this
    }

    private def printTypesOrBounds(types: List[TypeRepr], sep: String)(using elideThis: Option[Symbol]): this.type = {
      def printSeparated(list: List[TypeRepr]): Unit = list match {
        case Nil      =>
        case x :: Nil => printType(x)
        case x :: xs =>
          printType(x)
          this += sep
          printSeparated(xs)
      }
      printSeparated(types)
      this
    }

    private def printTargsDefs(targs: List[(TypeDef, TypeDef)], isDef: Boolean = true)(using elideThis: Option[Symbol]): Unit = {
      if (!targs.isEmpty) {
        def printSeparated(list: List[(TypeDef, TypeDef)]): Unit = list match {
          case Nil      =>
          case x :: Nil => printTargDef(x, isDef = isDef)
          case x :: xs =>
            printTargDef(x, isDef = isDef)
            this += ", "
            printSeparated(xs)
        }

        inSquare(printSeparated(targs))
      }
    }

    private def printTargDef(arg: (TypeDef, TypeDef), isMember: Boolean = false, isDef: Boolean = true)(using elideThis: Option[Symbol]): this.type = {
      val (argDef, argCons) = arg

      if (isDef) {
        if (argDef.symbol.flags.is(Flags.Covariant)) {
          this += highlightValDef("+")
        } else if (argDef.symbol.flags.is(Flags.Contravariant)) {
          this += highlightValDef("-")
        }
      }

      this += argCons.name
      argCons.rhs match {
        case rhs: TypeBoundsTree => printBoundsTree(rhs)
        case rhs: WildcardTypeTree =>
          printType(rhs.tpe)
        case rhs @ LambdaTypeTree(tparams, body) =>
          def printParam(t: Tree /*TypeTree | TypeBoundsTree*/ ): Unit = t match {
            case t: TypeBoundsTree => printBoundsTree(t)
            case t: TypeTree       => printTypeTree(t)
          }
          def printSeparated(list: List[TypeDef]): Unit = list match {
            case Nil =>
            case x :: Nil =>
              this += x.name
              printParam(x.rhs)
            case x :: xs =>
              this += x.name
              printParam(x.rhs)
              this += ", "
              printSeparated(xs)
          }
          inSquare(printSeparated(tparams))
          if (isMember) {
            body match {
              case MatchTypeTree(Some(bound), _, _) =>
                this += " <: "
                printTypeTree(bound)
              case _ =>
            }
            this += " = "
            printTypeOrBoundsTree(body)
          } else this
        case rhs: TypeTree =>
          this += " = "
          printTypeTree(rhs)
      }
    }

    private def printArgsDefs(args: List[ValDef])(using elideThis: Option[Symbol]): Unit = {
      // val args =
      //   argsRaw.filter(arg => {
      //     val isImplicit = arg.symbol.flags.is(Flags.Given) || arg.symbol.flags.is(Flags.Implicit)
      //     // only show implicit parameters if that is enabled
      //     !isImplicit || showDetails.showImplicitClauses
      //   })

      val argFlags = args match {
        case Nil      => Flags.EmptyFlags
        case arg :: _ => arg.symbol.flags
      }
      if (argFlags.is(Flags.Erased | Flags.Given)) {
        if (argFlags.is(Flags.Given)) this += " given"
        if (argFlags.is(Flags.Erased)) this += " erased"
        this += " "
      }
      inParens {
        if (argFlags.is(Flags.Implicit) && !argFlags.is(Flags.Given)) this += "implicit "

        def printSeparated(list: List[ValDef]): Unit = list match {
          case Nil      =>
          case x :: Nil => printParamDef(x)
          case x :: xs =>
            printParamDef(x)
            this += ", "
            printSeparated(xs)
        }

        printSeparated(args)
      }
    }

    private def printAnnotations(trees: List[Term])(using elideThis: Option[Symbol]): this.type = {
      def printSeparated(list: List[Term]): Unit = list match {
        case Nil      =>
        case x :: Nil => printAnnotation(x)
        case x :: xs =>
          printAnnotation(x)
          this += " "
          printSeparated(xs)
      }
      printSeparated(trees)
      this
    }

    private def printParamDef(arg: ValDef)(using elideThis: Option[Symbol]): Unit = {
      val name = splicedName(arg.symbol).getOrElse(arg.symbol.name)
      val sym = arg.symbol.ownerSafe
      if sym.isDefDef && sym.name == "<init>" then
        val ClassDef(_, _, _, _, body) = sym.ownerSafe.tree: @unchecked
        body.collectFirst {
          case vdef @ ValDef(`name`, _, _) if vdef.symbol.flags.is(Flags.ParamAccessor) =>
            if (!vdef.symbol.flags.is(Flags.Local)) {
              var printedPrefix = false
              if (vdef.symbol.flags.is(Flags.Override)) {
                this += "override "
                printedPrefix = true
              }
              printedPrefix |= printProtectedOrPrivate(vdef)
              if (vdef.symbol.flags.is(Flags.Mutable)) this += highlightValDef("var ")
              else if (printedPrefix || !vdef.symbol.flags.is(Flags.CaseAccessor)) this += highlightValDef("val ")
            }
        }
      end if

      this += highlightValDef(name) += ": "
      printTypeTree(arg.tpt)
    }

    private def printCaseDef(caseDef: CaseDef): this.type = {
      this += highlightValDef("case ")
      printPattern(caseDef.pattern)
      caseDef.guard match {
        case Some(t) =>
          this += " if "
          printTree(t)
        case None =>
      }
      this += highlightValDef(" =>")
      indented {
        caseDef.rhs match {
          case Block(stats, expr) =>
            printStats(stats, expr)(using None)
          case body =>
            this += lineBreak()
            printTree(body)
        }
      }
      this
    }

    private def printTypeCaseDef(caseDef: TypeCaseDef): this.type = {
      this += highlightValDef("case ")
      printTypeTree(caseDef.pattern)
      this += highlightValDef(" => ")
      printTypeTree(caseDef.rhs)
      this
    }

    private def printPattern(pattern: Tree): this.type = pattern match {
      case Wildcard() =>
        this += "_"

      case Bind(name, Wildcard()) =>
        this += name

      case Bind(name, Typed(Wildcard(), tpt)) =>
        this += highlightValDef(name) += ": "
        printTypeTree(tpt)

      case Bind(name, pattern) =>
        this += name += " @ "
        printPattern(pattern)

      case Unapply(fun, implicits, patterns) =>
        val fun2 = fun match {
          case TypeApply(fun2, _) => fun2
          case _                  => fun
        }
        fun2 match {
          case Select(extractor, "unapply" | "unapplySeq") =>
            printTree(extractor)
          case Ident("unapply" | "unapplySeq") =>
            this += fun.symbol.ownerSafe.fullName.stripSuffix("$")
          case _ =>
            throw new MatchError(fun.show(using Printer.TreeStructure))
        }
        inParens(printPatterns(patterns, ", "))

      case Alternatives(trees) =>
        inParens(printPatterns(trees, " | "))

      case TypedOrTest(tree1, tpt) =>
        tree1 match
          case Wildcard() =>
            this += "_: "
            printTypeTree(tpt)
          case _ =>
            printPattern(tree1)

      case v: Term =>
        printTree(v)

      case _ =>
        throw new MatchError(pattern.show(using Printer.TreeStructure))

    }

    inline private val qc = '\''
    inline private val qSc = '"'

    def printConstant(const: Constant): this.type = const match {
      case UnitConstant()     => this += highlightLiteral("()")
      case NullConstant()     => this += highlightLiteral("null")
      case BooleanConstant(v) => this += highlightLiteral(v.toString)
      case ByteConstant(v)    => this += highlightLiteral(v.toString)
      case ShortConstant(v)   => this += highlightLiteral(v.toString)
      case IntConstant(v)     => this += highlightLiteral(v.toString)
      case LongConstant(v)    => this += highlightLiteral(v.toString + "L")
      case FloatConstant(v)   => this += highlightLiteral(v.toString + "f")
      case DoubleConstant(v)  => this += highlightLiteral(v.toString)
      case CharConstant(v)    => this += highlightString(s"${qc}${escapedChar(v)}${qc}")
      case StringConstant(v)  => this += highlightString(s"${qSc}${escapedString(v)}${qSc}")
      case ClassOfConstant(v) =>
        this += "classOf"
        inSquare(printType(v))
    }

    private def printTypeOrBoundsTree(tpt: Tree)(using elideThis: Option[Symbol] = None): this.type = tpt match {
      case TypeBoundsTree(lo, hi) =>
        if (showDetails.showBoundsTypes)
          this += "_ >: "
          printTypeTree(lo)
          this += " <: "
          printTypeTree(hi)
        else
          this += "_"
      case tpt: WildcardTypeTree =>
        printType(tpt.tpe)
      case tpt: TypeTree =>
        printTypeTree(tpt)
    }

    /**
     * Print type tree
     *
     *  @param elideThis Shoud printing elide `C.this` for the given class `C`?
     *                   None means no eliding.
     *
     *   Self type annotation and types in parent list should elide current class
     *   prefix `C.this` to avoid type checking errors.
     */
    private def printTypeTree(tree: TypeTree)(using elideThis: Option[Symbol] = None): this.type = tree match {
      case Inferred() =>
        // TODO try to move this logic into `printType`
        def printTypeAndAnnots(tpe: TypeRepr): this.type = tpe match {
          case AnnotatedType(tp, annot) =>
            printTypeAndAnnots(tp)
            this += " "
            printAnnotation(annot)
          case tpe: TypeRef if tpe.typeSymbol == Symbol.requiredClass("scala.runtime.Null$") || tpe.typeSymbol == Symbol.requiredClass("scala.runtime.Nothing$") =>
            // scala.runtime.Null$ and scala.runtime.Nothing$ are not modules, those are their actual names
            printType(tpe)
          case tpe: TermRef if tpe.termSymbol.isClassDef && tpe.termSymbol.name.endsWith("$") =>
            printType(tpe)
            this += ".type"
          case tpe: TypeRef if tpe.typeSymbol.isClassDef && tpe.typeSymbol.name.endsWith("$") =>
            printType(tpe)
            this += ".type"
          case tpe @ TermRef(sym, _) =>
            printType(tpe)
            this += ".type"
          case tpe => printType(tpe)
        }
        printTypeAndAnnots(tree.tpe)

      case TypeIdent(name) =>
        printType(tree.tpe)

      case TypeSelect(qual, name) =>
        printTree(qual) += "." += highlightTypeDef(name)

      case TypeProjection(qual, name) =>
        printTypeTree(qual) += "#" += highlightTypeDef(name)

      case Singleton(ref) =>
        printTree(ref)
        ref match {
          case Literal(_) => this
          case _          => this += ".type"
        }

      case Refined(tpt, refinements) =>
        printTypeTree(tpt)
        inBlock(printTrees(refinements, "; "))

      case Applied(tpt, args) =>
        printTypeTree(tpt)
        inSquare(printTrees(args, ", "))

      case Annotated(tpt, annot) =>
        val Annotation(ref, args) = annot: @unchecked
        ref.tpe match {
          case tpe: TypeRef if tpe.typeSymbol == Symbol.requiredClass("scala.annotation.internal.Repeated") =>
            val Types.Sequence(tp) = tpt.tpe: @unchecked
            printType(tp)
            this += highlightTypeDef("*")
          case _ =>
            printTypeTree(tpt)
            this += " "
            printAnnotation(annot)
        }

      case MatchTypeTree(bound, selector, cases) =>
        printTypeTree(selector)
        this += highlightKeyword(" match ")
        inBlock(printTypeCases(cases, lineBreak()))

      case ByName(result) =>
        this += highlightTypeDef("=> ")
        printTypeTree(result)

      case LambdaTypeTree(tparams, body) =>
        printTargsDefs(tparams.zip(tparams), isDef = false)
        this += highlightTypeDef(" =>> ")
        printTypeOrBoundsTree(body)

      case TypeBind(name, _) =>
        this += highlightTypeDef(name)

      case TypeBlock(_, tpt) =>
        printTypeTree(tpt)

      case _ =>
        throw new MatchError(tree.show(using Printer.TreeStructure))

    }

    /**
     * Print type
     *
     *  @param elideThis Shoud printing elide `C.this` for the given class `C`?
     *                   None means no eliding.
     *
     *   Self type annotation and types in parent list should elide current class
     *   prefix `C.this` to avoid type checking errors.
     */
    def printType(tpe: TypeRepr)(using elideThis: Option[Symbol] = None): this.type = tpe match {
      case ConstantType(const) =>
        printConstant(const)

      case tpe: TypeRef =>
        val sym = tpe.typeSymbol
        if fullNames then
          tpe.qualifier match {
            case ThisType(tp) if tp.typeSymbol == defn.RootClass || tp.typeSymbol == defn.EmptyPackageClass =>
            case NoPrefix() =>
              if (sym.ownerSafe.flags.is(Flags.Package)) {
                // TODO should these be in the prefix? These are at least `scala`, `java` and `scala.collection`.
                val packagePath = sym.ownerSafe.fullName.stripPrefix("<root>").stripPrefix("<empty>").stripPrefix(".")
                if (packagePath != "")
                  this += packagePath += "."
              }
            case prefix: TermRef if prefix.termSymbol.isClassDef =>
              printType(prefix)
              this += "#"
            case prefix: TypeRef if prefix.typeSymbol.isClassDef =>
              printType(prefix)
              this += "#"
            case ThisType(TermRef(cdef, _)) if elideThis.nonEmpty && cdef == elideThis.get =>
            case ThisType(TypeRef(cdef, _)) if elideThis.nonEmpty && cdef == elideThis.get =>
            case prefix: TypeRepr =>
              printType(prefix)
              this += "."
          }
        this += highlightTypeDef(sym.name.stripSuffix("$"))

      case TermRef(prefix, name) =>
        if fullNames then
          prefix match {
            case NoPrefix() =>
              this += highlightTypeDef(name)
            case ThisType(tp) if tp.typeSymbol == defn.RootClass || tp.typeSymbol == defn.EmptyPackageClass =>
              this += highlightTypeDef(name)
            case _ =>
              printType(prefix)
              if (name != "package")
                this += "." += highlightTypeDef(name)
              this
          }
        else
          this += highlightTypeDef(name)

      case tpe @ Refinement(_, _, _) =>
        printRefinement(tpe)

      case AppliedType(tp, args) =>
        tp match {
          case tp: TypeLambda =>
            this += "("
            printType(tp)
            this += ")"
            inSquare(printTypesOrBounds(args, ", "))
          case tp: TypeRef if tp.typeSymbol == Symbol.requiredClass("scala.<repeated>") =>
            this += "_*"
          case _ =>
            printType(tp)
            inSquare(printTypesOrBounds(args, ", "))
        }

      case AnnotatedType(tp, annot) =>
        val Annotation(ref, args) = annot: @unchecked
        printType(tp)
        this += " "
        printAnnotation(annot)

      case AndType(left, right) =>
        printType(left)
        this += highlightTypeDef(" & ")
        printType(right)

      case OrType(left, right) =>
        printType(left)
        this += highlightTypeDef(" | ")
        printType(right)

      case MatchType(bound, scrutinee, cases) =>
        printType(scrutinee)
        this += highlightKeyword(" match ")
        inBlock(printTypes(cases, lineBreak()))

      case ByNameType(tp) =>
        this += highlightTypeDef(" => ")
        printType(tp)

      case ThisType(tp) =>
        tp match {
          case tp: TypeRef if !tp.typeSymbol.flags.is(Flags.Module) =>
            printFullClassName(tp)
            this += highlightTypeDef(".this")
          case TypeRef(prefix, name) if name.endsWith("$") =>
            if (fullNames) {
              prefix match {
                case NoPrefix()                                                                                 =>
                case ThisType(tp) if tp.typeSymbol == defn.RootClass || tp.typeSymbol == defn.EmptyPackageClass =>
                case _ =>
                  printType(prefix)
                  this += "."
              }
            }
            this += highlightTypeDef(name.stripSuffix("$"))
          case _ =>
            printType(tp)
        }

      case SuperType(thistpe, supertpe) =>
        printType(supertpe)
        this += highlightTypeDef(".super")

      case TypeLambda(paramNames, tparams, body) =>
        inSquare(printMethodicTypeParams(paramNames, tparams))
        this += highlightTypeDef(" => ")
        printType(body)

      case ParamRef(lambda, idx) =>
        lambda match {
          case MethodType(params, _, _) => this += params(idx)
          case PolyType(params, _, _)   => this += params(idx)
          case TypeLambda(params, _, _) => this += params(idx)
        }

      case RecursiveType(tpe) =>
        printType(tpe)

      case RecursiveThis(_) =>
        this += highlightTypeDef("this")

      case tpe: MethodType =>
        this += "("
        printList(tpe.paramNames.zip(tpe.paramTypes), ", ", (x: (String, TypeRepr)) => (this += x._1 += ": ").printType(x._2))
        this += ")"
        printType(tpe.resType)

      case tpe: PolyType =>
        this += "["
        printList(tpe.paramNames.zip(tpe.paramBounds), ", ", (x: (String, TypeBounds)) => (this += x._1 += " ").printType(x._2))
        this += "]"
        printType(tpe.resType)

      case tpe @ TypeBounds(lo, hi) =>
        if (showDetails.showBoundsTypes)
          this += "_ >: "
          printType(lo)
          this += " <: "
          printType(hi)
        else
          this += "_"

      case MatchCase(pat, rhs) =>
        this += "case "
        printType(pat)
        this += " => "
        printType(rhs)

      case _ =>
        throw new MatchError(tpe.show(using Printer.TypeReprStructure))
    }

    private def printSelector(sel: Selector): this.type = sel match {
      case SimpleSelector(name)          => this += name
      case OmitSelector(name)            => this += name += " => _"
      case RenameSelector(name, newName) => this += name += " => " += newName
      case GivenSelector(bound) =>
        bound match
          case Some(tpt) =>
            this += "given "
            printTree(tpt)
          case _ =>
            this += "given"
    }

    private def printDefinitionName(tree: Definition): this.type = tree match {
      case ValDef(name, _, _)         => this += highlightValDef(name)
      case DefDef(name, _, _, _)      => this += highlightValDef(name)
      case ClassDef(name, _, _, _, _) => this += highlightTypeDef(name.stripSuffix("$"))
      case TypeDef(name, _)           => this += highlightTypeDef(name)
    }

    private def printAnnotation(annot: Term)(using elideThis: Option[Symbol]): this.type = {
      val Annotation(ref, args) = annot: @unchecked
      this += "@"
      printTypeTree(ref)
      if (args.isEmpty)
        this
      else
        inParens(printTrees(args, ", "))
    }

    private def printDefAnnotations(definition: Definition)(using elideThis: Option[Symbol]): this.type = {
      val annots = definition.symbol.annotations.filter {
        case Annotation(annot, _) =>
          val sym = annot.tpe.typeSymbol
          sym != Symbol.requiredClass("scala.forceInline") &&
          sym.maybeOwner != Symbol.requiredPackage("scala.annotation.internal")
        case x => throw new MatchError(x.show(using Printer.TreeStructure))
      }
      printAnnotations(annots)
      if (annots.nonEmpty) this += " "
      else this
    }

    private def printRefinement(tpe: TypeRepr)(using elideThis: Option[Symbol]): this.type = {
      def printMethodicType(tp: TypeRepr): Unit = tp match {
        case tp @ MethodType(paramNames, params, res) =>
          inParens(printMethodicTypeParams(paramNames, params))
          printMethodicType(res)
        case tp @ TypeLambda(paramNames, params, res) =>
          inSquare(printMethodicTypeParams(paramNames, params))
          printMethodicType(res)
        case ByNameType(t) =>
          this += ": "
          printType(t)
        case tp: TypeRepr =>
          this += ": "
          printType(tp)
      }
      def rec(tp: TypeRepr): Unit = tp match {
        case Refinement(parent, name, info) =>
          rec(parent)
          indented {
            this += lineBreak()
            info match {
              case info: TypeBounds =>
                this += highlightKeyword("type ") += highlightTypeDef(name)
                printBounds(info)
              case ByNameType(_) | MethodType(_, _, _) | TypeLambda(_, _, _) =>
                this += highlightKeyword("def ") += highlightTypeDef(name)
                printMethodicType(info)
              case info: TypeRepr =>
                this += highlightKeyword("val ") += highlightValDef(name)
                printMethodicType(info)
            }
          }
        case tp =>
          printType(tp)
          this += " {"
      }
      rec(tpe)
      this += lineBreak() += "}"
    }

    private def printMethodicTypeParams(paramNames: List[String], params: List[TypeRepr])(using elideThis: Option[Symbol]): Unit = {
      def printInfo(info: TypeRepr) = info match {
        case info: TypeBounds => printBounds(info)
        case info: TypeRepr =>
          this += ": "
          printType(info)
      }
      def printSeparated(list: List[(String, TypeRepr)]): Unit = list match {
        case Nil =>
        case (name, info) :: Nil =>
          this += name
          printInfo(info)
        case (name, info) :: xs =>
          this += name
          printInfo(info)
          this += ", "
          printSeparated(xs)
      }
      printSeparated(paramNames.zip(params))
    }

    private def printBoundsTree(bounds: TypeBoundsTree)(using elideThis: Option[Symbol]): this.type = {
      bounds.low match {
        case Inferred() =>
        case low =>
          this += " >: "
          printTypeTree(low)
      }
      bounds.hi match {
        case Inferred() => this
        case hi =>
          this += " <: "
          printTypeTree(hi)
      }
    }

    private def printBounds(bounds: TypeBounds)(using elideThis: Option[Symbol]): this.type = {
      this += " >: "
      printType(bounds.low)
      this += " <: "
      printType(bounds.hi)
    }

    private def printProtectedOrPrivate(definition: Definition): Boolean = {
      var prefixWasPrinted = false
      def printWithin(within: TypeRepr) = within match {
        case TypeRef(_, name) => this += name
        case _                => printFullClassName(within)
      }
      if (definition.symbol.flags.is(Flags.Protected)) {
        this += highlightKeyword("protected")
        definition.symbol.protectedWithin match {
          case Some(within) =>
            inSquare(printWithin(within))
          case _ =>
        }
        prefixWasPrinted = true
      } else {
        definition.symbol.privateWithin match {
          case Some(within) =>
            this += highlightKeyword("private")
            inSquare(printWithin(within))
            prefixWasPrinted = true
          case _ =>
        }
      }
      if (prefixWasPrinted)
        this += " "
      prefixWasPrinted
    }

    private def printFullClassName(tp: TypeRepr): Unit = {
      def printClassPrefix(prefix: TypeRepr): Unit = prefix match {
        case TypeRef(prefix2, name) if fullNames =>
          printClassPrefix(prefix2)
          this += name += "."
        case _ =>
      }
      val TypeRef(prefix, name) = tp: @unchecked
      printClassPrefix(prefix)
      this += name
    }

    private def +=(x: Boolean): this.type = { sb.append(x); this }
    private def +=(x: Byte): this.type = { sb.append(x); this }
    private def +=(x: Short): this.type = { sb.append(x); this }
    private def +=(x: Int): this.type = { sb.append(x); this }
    private def +=(x: Long): this.type = { sb.append(x); this }
    private def +=(x: Float): this.type = { sb.append(x); this }
    private def +=(x: Double): this.type = { sb.append(x); this }
    private def +=(x: Char): this.type = { sb.append(x); this }
    private def +=(x: String): this.type = { sb.append(x); this }

    private def escapedChar(ch: Char): String = (ch: @switch) match {
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '"'  => "\\\""
      case '\'' => "\\\'"
      case '\\' => "\\\\"
      case _    => if ch.isControl then f"${"\\"}u${ch.toInt}%04x" else String.valueOf(ch).nn
    }

    private def escapedString(str: String): String = str flatMap escapedChar

    private[this] val names = collection.mutable.Map.empty[Symbol, String]
    private[this] val namesIndex = collection.mutable.Map.empty[String, Int]

    private def splicedName(sym: Symbol): Option[String] = {
      if sym.ownerSafe.isClassDef then None
      else
        names.get(sym).orElse {
          val name0 = sym.name
          val index = namesIndex.getOrElse(name0, 1)
          namesIndex(name0) = index + 1
          val name =
            if index == 1 then name0
            else s"`$name0${index.toString.toCharArray.nn.map { x => (x - '0' + '₀').toChar }.mkString}`"
          names(sym) = name
          Some(name)
        }
    }

    private object SpecialOp {
      def unapply(arg: Tree): Option[(String, List[Term])] = arg match {
        case arg @ Apply(fn, args) =>
          fn.tpe match {
            case tpe @ TermRef(ThisType(TypeRef(_, name)), name2) if name == "<special-ops>" =>
              Some((name2, args))
            case _ => None
          }
        case _ => None
      }
    }

    private object Annotation {
      def unapply(arg: Tree): Option[(TypeTree, List[Term])] = arg match {
        case New(annot)                                                  => Some((annot, Nil))
        case Apply(Select(New(annot), "<init>"), args)                   => Some((annot, args))
        case Apply(TypeApply(Select(New(annot), "<init>"), targs), args) => Some((annot, args))
        case _                                                           => None
      }
    }

    // TODO Provide some of these in scala.tasty.Reflection.scala and implement them using checks on symbols for performance
    private object Types {

      object Sequence {
        def unapply(tpe: TypeRepr): Option[TypeRepr] = tpe match {
          case AppliedType(seq, (tp: TypeRepr) :: Nil) if seq.typeSymbol == Symbol.requiredClass("scala.collection.Seq") || seq.typeSymbol == Symbol.requiredClass("scala.collection.immutable.Seq") =>
            Some(tp)
          case _ => None
        }
      }

      object Repeated {
        def unapply(tpe: TypeRepr): Option[TypeRepr] = tpe match {
          case AppliedType(rep, (tp: TypeRepr) :: Nil) if rep.typeSymbol == Symbol.requiredClass("scala.<repeated>") => Some(tp)
          case _                                                                                                     => None
        }
      }

    }

    private object PackageObject {
      def unapply(tree: Tree): Option[Tree] = tree match {
        case PackageClause(_, ValDef("package", _, _) :: body :: Nil) => Some(body)
        case _                                                        => None
      }
    }
  }
}
