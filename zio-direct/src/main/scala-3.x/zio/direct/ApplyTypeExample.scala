package zio.direct

import scala.quoted.*
import java.sql.SQLException

object FakeMarker {
  type A
  type B
  type C
}

object ApplyTypeExample {

  transparent inline def apply[F[_, _, _]](input: Any): Any = ${ impl[F]('input) }
  def impl[F[_, _, _]: Type](input: Expr[Any])(using q: Quotes): Expr[Any] = {
    import quotes.reflect._

    // ======== Seriously! This is all it should take! ========
    // val rebuiltType =
    //   tpe match
    //     case '[root[a, b, c]] =>
    //       TypeRepr.of[root]

    val stmt = '{ ???.asInstanceOf[F[FakeMarker.A, FakeMarker.B, FakeMarker.C]] }
    val tpe = stmt.asTerm.tpe
    val root =
      tpe match
        case AppliedType(root, List(a, b, c)) =>
          println(s"Root: ${root.show}. Others: ${a.show}, ${b.show}, ${c.show}")
          root
        case _ =>
          report.errorAndAbort("No match")

    val rebuiltType =
      AppliedType(
        root,
        List(TypeRepr.of[String], TypeRepr.of[SQLException], TypeRepr.of[Long])
      )
    println(rebuiltType.show)

    rebuiltType.asType match
      case '[rt] =>
        '{ $input.asInstanceOf[rt] }
  }
}
