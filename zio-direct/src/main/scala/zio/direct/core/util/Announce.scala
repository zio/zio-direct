package zio.direct.core.util

import zio.direct.core.util.Announce.FileShow.FullPath

object Announce {
  sealed trait Volume
  object Volume {
    case object Loud extends Volume
    case object Normal extends Volume
  }

  sealed trait FileShow
  object FileShow {
    case class FullPath(str: String) extends FileShow
    case object None extends FileShow
  }

  def section(heading: String, section: String = "", fileShow: FileShow, volume: Volume = Volume.Normal) = {
    import zio.direct.core.util.IndentExt._
    val headingText =
      volume match {
        case Volume.Loud =>
          s"******* $heading *************************"
        case _ =>
          s"=== $heading ========"
      }

    val fileStringOpt =
      fileShow match {
        case FullPath(str) => Some(s"[${str}]")
        case _             => None
      }

    val text =
      List(
        Some(headingText),
        fileStringOpt,
        (if (section != "") Some(section.indent(1) + "\n") else None)
      ).collect {
        case Some(value) => value
      }.mkString("\n")
    println(text)
  }
}
