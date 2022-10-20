package io.monadless.core.metaprog

import scala.quoted._
import io.monadless.core.metaprog.Extractors.Lambda1

object ValDefExtractor:

  object ValDefAsLambda {
      def unapply(using Quotes)(stmt: quotes.reflect.Statement) =
        import quotes.reflect._
        ValDefAsLambda.Term.unapply(stmt)

      object Term:
        def unapply(using Quotes)(stmt: quotes.reflect.Statement): Option[(Expr[? => ?], Expr[?])] =
          import quotes.reflect._
          stmt match {
            case valdef @ ValDef(name, tpe, rhsOpt) =>
              val actualBody =
                rhsOpt match {
                  // TODO Better site-description in error
                  case None      => report.throwError(s"Cannot parse 'val' clause with no '= rhs' (i.e. equals and right hand side) of ${Printer.TreeStructure.show(stmt)}")
                  case Some(rhs) => rhs
                }


              val mtpe = MethodType(List(name))(_ => List(actualBody.tpe), _ => actualBody.tpe)
              val lam =
                Lambda(stmt.symbol.owner, mtpe, {
                  case (methSym, List(name: Term)) =>
                    actualBody.changeOwner(methSym)
                  }
                )

              println(s"========== Show ${lam.show}")

              val transformedBody =
                lam.asExpr match {
                  case Lambda1(varName, tpe, body) => body
                  case _ => report.throwError("malformed lambda")
                }

              Some((lam.asExprOf[? => ?], transformedBody))

            case _ => None
          }
    }
