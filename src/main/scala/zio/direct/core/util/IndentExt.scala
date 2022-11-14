package zio.direct.core.util

object IndentExt {
  implicit class StringOpsExt(str: String) {
    def indent(indentNum: Int, prefix: String = " "): String =
      str.split("\n").map(elem => makePrefix(indentNum) + prefix + elem).mkString("\n")
  }

  private def makePrefix(num: Int) = indentOf(num)

  private def indentOf(num: Int) =
    (0 to num).map(_ => "").mkString("  ")
}
