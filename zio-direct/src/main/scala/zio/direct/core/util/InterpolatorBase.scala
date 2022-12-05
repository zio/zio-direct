package zio.direct.core.util

import java.io.PrintStream
import zio.direct.core.metaprog.Instructions
import scala.util.matching.Regex
import scala.collection.mutable.ArrayBuffer

trait WithInterpolatorBase {

  // To inject formatter for specific scala dialect
  def printAny(any: Any): String

  private object IndentUtil {
    implicit class StringOpsExt(str: String) {
      def fitsOnOneLine: Boolean = !str.contains("\n")
      def multiline(indent: Int, prefix: String): String =
        str.split("\n").map(elem => indent.prefix + prefix + elem).mkString("\n")
    }

    implicit class IndentOps(i: Int) {
      def prefix = indentOf(i)
    }

    private def indentOf(num: Int) =
      (0 to num).map(_ => "").mkString("  ")
  }
  import IndentUtil._

  object Interpolator {
    def apply(traceType: TraceType)(instructions: Instructions) =
      new Interpolator(traceType, instructions)
  }

  class Interpolator private (
      traceType: TraceType,
      instructions: Instructions,
      defaultIndent: Int = 0,
      out: PrintStream = System.out
  ) {
    implicit class InterpolatorExt(sc: StringContext) {
      def trace(elements: Any*) = new Traceable(sc, elements)
    }

    class Traceable(sc: StringContext, elementsSeq: Seq[Any]) {

      private val elementPrefix = "|  "

      private sealed trait PrintElement
      private case class Str(str: String, first: Boolean) extends PrintElement
      private case class Elem(value: String) extends PrintElement
      private case class Simple(value: String) extends PrintElement
      private case object Separator extends PrintElement

      implicit class StrOps(str: String) {
        def reallyFitsOnOneLine: Boolean = {
          !str.contains("\n") && !str.contains("\r")
        }
        def reallyMultiline(indent: Int, prefix: String, prependNewline: Boolean = false): String = {
          // Split a string and indent.... if it is actually multi-line. Since this typically is used
          // on parts of strings of a parent-string which may be multline, but the individual elements
          // might not be which results in strange things like:
          //    (Node Option) ['mt] Mapping: asExprOf:       |lastName      | into       |String      | in       |(
          //    |    (optField: Option[LastNameAge]) =>
          //    |      optField.map[String](((prop: LastNameAge) => prop.lastName))
          //    |)      |
          val prepend = if (prependNewline) "\n" else ""
          if (str.contains("\n"))
            prepend + str.split("\n").map(elem => indent.prefix + prefix + elem).mkString("\n")
          else
            str
        }
      }

      private def generateStringForCommand(value: Any, indent: Int) = {
        val objectString = printAny(value)
        val oneLine = objectString.reallyFitsOnOneLine
        oneLine match {
          case true => s"${indent.prefix}> ${objectString}"
          case false =>
            s"${indent.prefix}>\n${objectString.reallyMultiline(indent, elementPrefix)}"
        }
      }

      private def readFirst(first: String) =
        new Regex("%([0-9]+)(.*)").findFirstMatchIn(first) match {
          case Some(matches) =>
            (matches.group(2).trim, Some(matches.group(1).toInt))
          case None => (first, None)
        }

      sealed trait Splice { def value: String }
      object Splice {
        case class Simple(value: String) extends Splice // Simple splice into the string, don't indent etc...
        case class Show(value: String) extends Splice // Indent, colorize the element etc...
      }

      private def readBuffers() = {
        def orZero(i: Int): Int = if (i < 0) 0 else i

        val parts = sc.parts.iterator.toList
        val elements = elementsSeq.toList.map(elem => {
          if (elem.isInstanceOf[String]) Splice.Simple(elem.asInstanceOf[String])
          else Splice.Show(printAny(elem))
        })

        val (firstStr, explicitIndent) = readFirst(parts.head)
        val indent =
          explicitIndent match {
            case Some(value) => value
            case None => {
              // A trick to make nested calls of andReturn indent further out which makes andReturn MUCH more usable.
              // Just count the number of times it has occurred on the thread stack.
              val returnInvocationCount = Thread
                .currentThread()
                .getStackTrace
                .toList
                .count(e => e.getMethodName.contains("andReturn") || e.getMethodName.contains("andContinue"))
              defaultIndent + orZero(returnInvocationCount - 1) * 2
            }
          }

        val partsIter = parts.iterator
        partsIter.next() // already took care of the 1st element
        val elementsIter = elements.iterator

        val sb = new ArrayBuffer[PrintElement]()
        sb.append(Str(firstStr, true))

        while (elementsIter.hasNext) {
          val nextElem = elementsIter.next()
          nextElem match {
            case Splice.Simple(v) =>
              sb.append(Simple(v))
              val nextPart = partsIter.next()
              sb.append(Simple(nextPart))
            case Splice.Show(v) =>
              sb.append(Separator)
              sb.append(Elem(v))
              val nextPart = partsIter.next()
              sb.append(Separator)
              sb.append(Str(nextPart, false))
          }
        }

        (sb.toList, indent)
      }

      def generateString() = {
        val (elementsRaw, indent) = readBuffers()

        val elements = elementsRaw.filter {
          case Str(value, _) => value.trim != ""
          case Elem(value)   => value.trim != ""
          case _             => true
        }

        val oneLine = elements.forall {
          case Simple(value) => value.reallyFitsOnOneLine
          case Elem(value)   => value.reallyFitsOnOneLine
          case Str(value, _) => value.reallyFitsOnOneLine
          case _             => true
        }

        val output =
          elements.map {
            case Simple(value) if (oneLine)     => value
            case Str(value, true) if (oneLine)  => indent.prefix + value
            case Str(value, false) if (oneLine) => value
            case Elem(value) if (oneLine)       => value
            case Separator if (oneLine)         => " "
            case Simple(value)                  => value.reallyMultiline(indent, "|", true)
            case Str(value, true)               => indent.prefix + value.reallyMultiline(indent, "", true)
            case Str(value, false)              => value.reallyMultiline(indent, "|", true)
            case Elem(value)                    => value.reallyMultiline(indent, "|  ", true)
            case Separator                      => "\n"
          }

        (output.mkString, indent)
      }

      def tracesEnabled = instructions.traceTypes.contains(traceType)

      private def logIfEnabled[T]() =
        if (tracesEnabled)
          Some(generateString())
        else
          None

      def andLog(): Unit =
        logIfEnabled().foreach(value => out.println(value._1))

      def andContinue[T](command: => T) = {
        logIfEnabled().foreach(value => out.println(value._1))
        command
      }

      def andReturn[T](command: => T) = {
        logIfEnabled() match {
          case Some((output, indent)) =>
            // do the initial log
            out.println(output)
            // evaluate the command, this will activate any traces that were inside of it
            val result = command
            out.println(generateStringForCommand(result, indent))

            result
          case None =>
            command
        }
      }

      def andReturnLog[T, L](command: => (T, L)) = {
        logIfEnabled() match {
          case Some((output, indent)) =>
            // do the initial log
            out.println(output)
            // evaluate the command, this will activate any traces that were inside of it
            val (result, logData) = command
            out.println(generateStringForCommand(logData, indent))

            result
          case None =>
            command
        }
      }

      def andReturnIf[T](command: => T)(showIf: T => Boolean) = {
        logIfEnabled() match {
          case Some((output, indent)) =>
            // Even though we usually want to evaluate the command after the initial log was done
            // (so that future logs are nested under this one after the intro text but not
            // before the return) but we cann't do that in this case because the switch indicating
            // whether to output anything or not is dependant on the return value.
            val result = command

            if (showIf(result))
              out.println(output)

            if (showIf(result))
              out.println(generateStringForCommand(result, indent))

            result
          case None =>
            command
        }
      }
    }
  }

}
