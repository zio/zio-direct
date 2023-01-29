package zio.direct.core.metaprog

import scala.quoted._
import pprint._
import fansi.Str
import zio.direct.core.util.Format
import zio.direct.core.norm.WithComputeType
import zio.direct.core.util.WithInterpolator
import zio.direct.core.util.WithUnsupported

trait WithPrintIR {
  self: WithF with WithIR with WithZioType with WithUnsupported =>

  implicit val macroQuotes: Quotes
  import macroQuotes.reflect

  def PrintAny(model: Any) =
    Format(new PrintIR().apply(model).plainText)

  def PrintIR(model: IR) =
    Format(new PrintIR().apply(model).plainText)

  def PrintIR(model: IRT) =
    Format(new PrintIR().apply(model).plainText)

  private class PrintIR extends pprint.Walker {
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

        // special handling for the IR.Monad case because it's not a case class (since it has "trivia properties")
        case m: IR.Monad  => Tree.Apply("IR.Monad", Iterator(treeify(m.code), treeify(m.source)))
        case m: IRT.Monad => Tree.Apply("IRT.Monad", Iterator(treeify(m.code), treeify(m.source)))

        case v: IR.Parallel  => Tree.Apply("IR.Parallel", Iterator(treeify(v.monads), treeify(v.body)))
        case v: IRT.Parallel => Tree.Apply("IRT.Parallel", Iterator(treeify(v.monads), treeify(v.body)))

        // Need to ignore the 'orig' code element of ValDef since it would add too much info to the tree
        case v: IR.ValDef  => Tree.Apply("IR.ValDef", Iterator(treeify(v.symbol), treeify(v.assignment), treeify(v.bodyUsingVal)))
        case v: IRT.ValDef => Tree.Apply("IRT.ValDef", Iterator(treeify(v.symbol), treeify(v.assignment), treeify(v.bodyUsingVal)))

        // append "IR." to all IR-members
        case m: IR =>
          super.treeify(m) match {
            case Tree.Apply(prefix, body) => Tree.Apply(s"IR.${prefix}", body)
            case other                    => other
          }
        case m: IRT =>
          super.treeify(m) match {
            case Tree.Apply(prefix, body) => Tree.Apply(s"IRT.${prefix}", body)
            case other                    => other
          }

        case zt: ZioType =>
          Tree.Literal(s"ZioType[${Format.TypeRepr(zt.r)}, ${Format.TypeRepr(zt.e)}, ${Format.TypeRepr(zt.a)}]")

        case _ =>
          super.treeify(x)
      }

    def apply(x: Any): fansi.Str = {
      fansi.Str.apply(this.tokenize(x).toSeq: _*)
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
