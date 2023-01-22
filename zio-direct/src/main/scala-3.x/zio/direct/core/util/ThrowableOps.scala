package zio.direct.core.util

import java.io.ByteArrayOutputStream

object ThrowableOps:
  def printStackTrace: String =
    new RuntimeException().stackTraceToString

  extension (t: Throwable)
    def stackTraceToString =
      val stream = new ByteArrayOutputStream()
      val writer = new java.io.BufferedWriter(new java.io.OutputStreamWriter(stream))
      t.printStackTrace(new java.io.PrintWriter(writer))
      writer.flush
      stream.toString
