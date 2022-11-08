package zio.asyncawait.core.metaprog

import scala.quoted._
import pprint._
import fansi.Str
import zio.asyncawait.core.util.Format

trait ModelPrinting extends Model {
  implicit val macroQuotes: Quotes
  import macroQuotes.reflect

  def mprint(model: IR) =
    Format(new AstPrinter().apply(model).plainText)

  class AstPrinter extends pprint.Walker {
    val defaultWidth: Int = 150
    val defaultHeight: Int = Integer.MAX_VALUE
    val defaultIndent: Int = 2
    val colorLiteral: fansi.Attrs = fansi.Color.Green
    val colorApplyPrefix: fansi.Attrs = fansi.Color.Yellow
    def escapeUnicode = false
    override def showFieldNames = false

    override def additionalHandlers: PartialFunction[Any, Tree] = PartialFunction.empty

    override def treeify(x: Any): Tree =
      x match {
        // Use short-code printer to print actual Scala tree nodes
        case someTree if (x.getClass().getName().contains("dotty.tools.dotc.ast.Trees")) =>
          val tree = someTree.asInstanceOf[reflect.Tree]
          // TODO Formatter uses scalafmt here in Format.Tree and on the top level to format
          //      the whole thing afterward, can optimize by only doing it once
          Tree.Apply("SCALA", Iterator(Tree.Literal(s"{${Format.Tree(tree)}}")))

        case someSymbol if (x.getClass().getName().contains("dotty.tools.dotc.core.Symbols")) =>
          val sym = someSymbol.asInstanceOf[reflect.Symbol]
          // Need to tell the formatter to ignore these things because formatting will fail if it doesn't
          // so we put them into a scala comment, should figure out some better way to do it eventually.
          if (sym.isNoSymbol)
            Tree.Literal("NO_SYMBOL")
          else
            Tree.Apply("SCALA_SYM", Iterator(Tree.Literal(s"/*${sym}*/")))

        case _ =>
          //println(s"CLASS OF: ${x.getClass()}")
          super.treeify(x)
      }

    private def TreeApplyList(prefix: String, body: List[Tree]) = Tree.Apply(prefix, body.iterator)

    private def l(trees: Tree*): List[Tree] = List[Tree](trees: _*)

    def apply(x: Any): fansi.Str = {
      fansi.Str.join(this.tokenize(x).toSeq: _*)
    }

    def tokenize(x: Any): Iterator[fansi.Str] = {
      val tree = this.treeify(x)
      val renderer = new Renderer(defaultWidth, colorApplyPrefix, colorLiteral, defaultIndent)
      val rendered = renderer.rec(tree, 0, 0).iter
      val truncated = new Truncated(rendered, defaultWidth, defaultHeight)
      truncated
    }
  }
}

trait Model {
  implicit val macroQuotes: Quotes
  import macroQuotes.reflect._

  protected sealed trait IR
  protected object IR {
    sealed trait Monadic extends IR

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
    case class Match(scrutinee: Monadic | Pure, caseDefs: List[IR.Match.CaseDef]) extends Monadic
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
}
