package zio.direct.core.metaprog

import zio.direct.core.util.WithFormat
import pprint._
import scala.annotation.nowarn

trait WithPrintIR extends MacroBase {
  self: WithIR with WithZioType with WithFormat =>

  import c.universe.{Tree => STree, Type => SType, _}

  def PrintAny(model: Any) =
    Format(new PrintIR().apply(model).plainText)

  def PrintIR(model: IR) =
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

    @nowarn
    override def treeify(x: Any): Tree =
      x match {
        // Use short-code printer to print actual Scala tree nodes
        case someTree if (x.getClass().getName().contains("scala.reflect.internal.Trees")) =>
          val tree = someTree.asInstanceOf[STree]
          // TODO Formatter uses scalafmt here in Format.Tree and on the top level to format
          //      the whole thing afterward, can optimize by only doing it once
          Tree.Apply("SCALA", Iterator(Tree.Literal(s"{${Format.Tree(tree)}}")))

        case tpeTree if (x.getClass().getName().contains("scala.reflect.internal.Types")) =>
          val tpe = tpeTree.asInstanceOf[SType]
          Tree.Apply("SCALA_Type", Iterator(Tree.Literal(s"{${Format.Type(tpe)}}")))

        case someSymbol if (x.getClass().getName().contains("scala.reflect.internal.Symbols")) =>
          val sym = someSymbol.asInstanceOf[Symbol]
          if (sym == NoSymbol)
            Tree.Literal("NO_SYMBOL")
          else
            Tree.Apply("SCALA_SYM", Iterator(Tree.Literal(s"/*${sym}*/")))

        case someSymbol if (x.getClass().getName().contains("scala.reflect.internal.Names$TermName")) =>
          val sym = someSymbol.asInstanceOf[TermName]
          Tree.Apply("TERM_NAME", Iterator(Tree.Literal(s"/*${sym}*/")))

        // special handling for the IR.Monad case because it's not a case class (since it has "trivia properties")
        case m: IR.Monad =>
          Tree.Apply("IR.Monad", Iterator(treeify(m.code), treeify(m.source)))

        case v: IR.Parallel =>
          Tree.Apply("IR.Parallel", Iterator(treeify(v.monads), treeify(v.body)))

        // Need to ignore the 'orig' code element of ValDef since it would add too much info to the tree
        case v: IR.ValDef =>
          Tree.Apply("IR.ValDef", Iterator(treeify(v.symbol), treeify(v.assignment), treeify(v.bodyUsingVal)))

        // append "IR." to all IR-members
        case m: IR =>
          super.treeify(m) match {
            case Tree.Apply(prefix, body) => Tree.Apply(s"IR.${prefix}", body)
            case other                    => other
          }

        case zt: ZioType =>
          Tree.Literal(s"ZioType[${Format.Type(zt.r)}, ${Format.Type(zt.e)}, ${Format.Type(zt.a)}]")

        case _ =>
          super.treeify(x)
      }

    def apply(x: Any): fansi.Str = {
      fansi.Str.join(this.tokenize(x).toSeq)
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
