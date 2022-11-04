package zio.asyncawait.core.util

import scala.quoted._
import zio.ZIO

object ComputeTotalZioType {
  def of(using Quotes)(terms: quotes.reflect.Term*) =
    (environmentOf(terms: _*).asType, errorOf(terms: _*).asType, valueOf(terms: _*).asType)

  def asTypeRepr(using Quotes)(terms: quotes.reflect.Term*) =
    (environmentOf(terms: _*), errorOf(terms: _*), valueOf(terms: _*))

  // Assuming it is a non-empty list
  def valueOf(using Quotes)(terms: quotes.reflect.Term*) =
    import quotes.reflect._
    terms.drop(1).foldLeft(terms.head.tpe)((tpe, additionalTerm) =>
      tpe.asType match
        case '[ZIO[x, y, a]] =>
          additionalTerm.tpe.asType match
            case '[ZIO[x1, y1, b]] =>
              TypeRepr.of[a with b]
    )

  def errorOf(using Quotes)(terms: quotes.reflect.Term*) =
    import quotes.reflect._
    terms.drop(1).foldLeft(terms.head.tpe)((tpe, additionalTerm) =>
      tpe.asType match
        case '[ZIO[x, y, a]] =>
          additionalTerm.tpe.asType match
            case '[ZIO[x1, y1, b]] =>
              TypeRepr.of[y with y1]
    )

  def environmentOf(using Quotes)(terms: quotes.reflect.Term*) =
    import quotes.reflect._
    terms.drop(1).foldLeft(terms.head.tpe)((tpe, additionalTerm) =>
      tpe.asType match
        case '[ZIO[x, y, a]] =>
          additionalTerm.tpe.asType match
            case '[ZIO[x1, y1, b]] =>
              TypeRepr.of[x with x1]
    )
}