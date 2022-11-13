package zio.asyncawait.core.metaprog

import scala.quoted._
import pprint._
import fansi.Str
import zio.asyncawait.core.util.Format

trait ModelPrinting {
  self: Model =>

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

        case tpeTree if (x.getClass().getName().contains("dotty.tools.dotc.core.Types")) =>
          val tpe = tpeTree.asInstanceOf[reflect.TypeRepr]
          Tree.Apply("SCALA_Type", Iterator(Tree.Literal(s"{${Format.TypeRepr(tpe)}}")))

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