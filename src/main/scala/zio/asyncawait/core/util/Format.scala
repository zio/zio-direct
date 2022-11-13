package zio.asyncawait.core.util

import scala.util.{Try, Success, Failure}
import scala.quoted._
import io.getquill.util.ScalafmtFormat
import zio.asyncawait.core.metaprog.Trees
import zio.asyncawait.core.metaprog.Extractors.Seal
import scala.meta.internal.javacp.BaseType.S

/** Facade objects to make display of zio flatMap simpler */
object ZioFacade {
  type Bounds

  object ZIO {
    def succeed[T](any: T): zio.ZIO[Any, Throwable, T] = ???
    def service[T]: zio.URIO[T, T] = ???
    def attempt[T](any: T): zio.Task[T] = ???
  }

  private trait AnyToNothing

  def makeFacade(using q: Quotes)(tree: quotes.reflect.Tree): q.reflect.Tree =
    import quotes.reflect._
    (new TreeMap:
        // want to remove noise from ZIO[_ >: Nothing <: Any, _ >: Nothing <: Any, _ >: Nothing <: Any]
        // this doesn't seem to do it though
        override def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree = {
          // val str = Printer.TreeShortCode.show(tree)
          // val isTypeBoundsTpe =
          // if (str.contains("_ >: Nothing <: Any"))
          //   println(
          //    s"""|========== (transformTypeTree) Looking at type tree
          //        |(Is Zio Type: ${IsZioType.unapply(tree.tpe).map(Format.TypeRepr(_))})
          //        |${Printer.TreeShortCode.show(tree)}"
          //     """.stripMargin
          //   )

          tree match
            // case tree: Applied =>
            //   Applied.copy(tree)(transformTypeTree(tree.tpt)(owner), transformTrees(tree.args)(owner))

            //case HasTypeBoundsType(_) => TypeTree.of[AnyToNothing]
            //case TypeOfTypeTree(CanSimplifyZioType(tpe)) => TypeTree.of(using tpe.asType)

            case _: Tree => super.transformTypeTree(tree)(owner)
        }

        // need to check for type-bounds trees in order to re-write them into a nicer syntax
        // e.g. lots of `ZIO[_ >: Nothing <: Any, _ >: Nothing <: Any, _ >: Nothing <: Any]`
        // gets very verbose. This will appear as a Tree object but not always as a TreeType.
        // I can also appear inside a TypeBoundsTree object and potentially other Tree types
        // so need to pull checking the .tpe of whatever type it is into here.
        private object HasTypeBoundsType {
          def unapply(tree: Tree) =
            TypeOfTypeTree.unapply(tree) match {
              case Some(b @ IsTypeBounds(_)) => Some(b)
              case _ => None
            }
        }

        object TypeOfTypeTree {
          def unapply(tree: Tree) =
            tree match {
              case v: TypeTree => Some(v.tpe)
              case v: TypeBoundsTree => Some(v.tpe)
              case _ => None
            }
        }

        object IsTypeBounds {
          def unapply(tpe: TypeRepr) =
            tpe match {
              case b @ TypeBounds(_, _) => Some(b)
              case _ => None
            }
        }

        // Remove ZIO[_ >: Nothing <: Any, _ >: Nothing <: Any, _ >: Nothing <: Any] instances.
        // For some reason, matching on TypeBoundsTree doesn't always work and we have to re-parse zios directly.
        // TODO does some strainge things in certain cases, maybe not do .simplified? need to look into it
        private object CanSimplifyZioType {
          def unapply(tpe: TypeRepr) =
            tpe.asType match {
              case '[zio.ZIO[r, e, a]] =>
                (TypeRepr.of[r].simplified.asType, TypeRepr.of[e].simplified.asType, TypeRepr.of[a].simplified.asType) match {
                  case ('[r1], '[e1], '[a1]) =>
                    Some(TypeRepr.of[zio.ZIO[r1, e1, a1]])
                }
              case _ => None
            }
        }


        override def transformTree(tree: Tree)(owner: Symbol): Tree = {
          tree match {
            //case HasTypeBoundsType(bounds) => TypeTree.of[AnyToNothing]
            case _ => super.transformTree(tree)(owner)
          }
        }

        override def transformTerm(tree: Term)(owner: Symbol): Term = {
          tree match
            case Seal('{ zio.ZIO.succeed[t]($tt)($impl) }) =>
              '{ ZIO.succeed[t](${transformTerm(tt.asTerm)(owner).asExprOf[t]}) }.asTerm
            case Seal('{ zio.ZIO.attempt[t]($tt)($impl) }) =>
              '{ ZIO.attempt[t](${transformTerm(tt.asTerm)(owner).asExprOf[t]}) }.asTerm
            case Seal('{ zio.ZIO.service[t]($impl, $impl2) }) =>
              '{ ZIO.service[t] }.asTerm
            case _: Term =>
              super.transformTerm(tree)(owner)
        }
      ).transformTree(tree)(Symbol.spliceOwner)
}

object Format {
  // import org.scalafmt.interfaces.Scalafmt
  // import org.scalafmt.cli.Scalafmt210
  object TypeOf {
    def apply[T: Type](using Quotes) =
      import quotes.reflect._
      Format.Type(summon[Type[T]])
  }

  object TypeRepr {
    // since we need a qctx to be able to actually pass in a TypeRepr element, it's not possible
    // (unless lampepfl/dotty#10689 is resolved) to create a global module that does TypeRepr formatting. This is a bit
    // of a hacky way around that that just requires the element to be an inner class of a Quotes instance
    // and the casts it to the specific Quotes insance. Should reconsider this when lampepfl/dotty#10689 is fixed.
    def apply(typeRepr: Quotes#reflectModule#TypeRepr)(using qctx: Quotes) =
      import qctx.reflect._
      Printer.TypeReprShortCode.show(typeRepr.asInstanceOf[qctx.reflect.TypeRepr])
  }

  object Term:
    def apply(term: Quotes#reflectModule#Term)(using qctx: Quotes) =
      import qctx.reflect._
      printShortCode(term.asInstanceOf[qctx.reflect.Term])

  object Tree:
    def apply(term: Quotes#reflectModule#Tree)(using qctx: Quotes) =
      import qctx.reflect._
      printShortCode(term.asInstanceOf[qctx.reflect.Tree])

  object TermRaw:
    def apply(term: Quotes#reflectModule#Term)(using qctx: Quotes) =
      import qctx.reflect._
      Printer.TreeStructure.show(term.asInstanceOf[qctx.reflect.Term])

  /** Same as TypeRepr but also widens the type since frequently types are singleton i.e. 'person.name' has the type 'name' as opposed to String */
  object TypeReprW {
    def apply(typeRepr: Quotes#reflectModule#TypeRepr)(using qctx: Quotes) =
      import qctx.reflect._
      Printer.TypeReprShortCode.show(typeRepr.asInstanceOf[qctx.reflect.TypeRepr].widen)
  }

  object Type {
    def apply(tpe: scala.quoted.Type[_])(using Quotes) =
      import quotes.reflect._
      tpe match
        case '[tt] => Printer.TypeReprShortCode.show(quotes.reflect.TypeRepr.of[tt])
        case _     => tpe
  }

  object Expr {
    def apply(expr: Expr[_], showErrorTrace: Boolean = false)(using Quotes) =
      import quotes.reflect._
      Format(printShortCode(expr.asTerm), showErrorTrace)

    def Detail(expr: Expr[_])(using Quotes) =
      import quotes.reflect._
      val term = expr.asTerm
      // if (ProtoMessages.errorDetail) {
      //   s"""|
      //       |s"==== Expression ====
      //       |  ${Format(Printer.TreeShortCode.show(term))}
      //       |==== Extractors ===
      //       |  ${Format(Printer.TreeStructure.show(term))}
      //       |""".stripMargin
      // } else {
      //   Format(Printer.TreeShortCode.show(term))
      // }
      Format(Printer.TreeShortCode.show(term))
  }

  private def printShortCode(using Quotes)(code: quotes.reflect.Tree): String =
    import quotes.reflect._
    Printer.TreeShortCode.show(ZioFacade.makeFacade(code))

  private def printShortCode(using Quotes)(expr: Expr[_]): String =
    import quotes.reflect._
    printShortCode(expr.asTerm)

  def apply(code: String, showErrorTrace: Boolean = true) = {
    val encosedCode =
      s"""|object DummyEnclosure {
            |  ${code}
            |}""".stripMargin

    // NOTE: Very ineffifient way to get rid of DummyEnclosure on large blocks of code
    //       use only for debugging purposes!
    def unEnclose(enclosedCode: String) =
      val lines =
        enclosedCode
          .replaceFirst("^object DummyEnclosure \\{", "")
          .reverse
          .replaceFirst("\\}", "")
          .reverse
          .split("\n")
      val linesTrimmedFirst = if (lines.head == "") lines.drop(1) else lines
      // if there was a \n} on the last line, remove the }
      val linesTrimmedLast = if (linesTrimmedFirst.last == "") linesTrimmedFirst.dropRight(1) else linesTrimmedFirst
      // then if all lines had at least one indent i.e. "  " remove that
      if (linesTrimmedLast.forall(line => line.startsWith("  ")))
        linesTrimmedLast.map(line => line.replaceFirst("  ", "")).mkString("\n")
      else
        linesTrimmedLast.mkString("\n")

    val formatted =
      Try {
        // val formatCls = classOf[ScalafmtFormat.type]
        // val result = formatCls.getMethod("apply").invoke(null, encosedCode)
        // println("============ GOT HERE ===========")
        // val resultStr = s"${result}"
        // resultStr
        ScalafmtFormat(
          // Various other cleanup needed to make the formatter happy
          encosedCode
            .replace("_*", "_")
            .replace("_==", "==")
            .replace("_!=", "!=")
            //.replaceAll("\\(evidence\\$([0-9]+): (zio\\.)?Unsafe\\) \\?=> ", "")
            ,
          showErrorTrace
        )
      }.getOrElse {
        println("====== WARNING: Scalafmt Not Detected ====")
        encosedCode
      }

    unEnclose(formatted)
  }
}
