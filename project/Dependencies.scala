import sbt.Keys.scalaVersion
import sbt._

object Dependencies {
  val ZioVersion = "2.0.2"
  val PPrintVersion = "0.6.6"
  val CompatVersion = "1.0.1"
  val QuillUtilVersion = "4.6.0"

  val zio               = "dev.zio" %% "zio"             % ZioVersion
  val `zio-test`        = "dev.zio" %% "zio-test"        % ZioVersion % "test"
  val `zio-test-sbt`    = "dev.zio" %% "zio-test-sbt"    % ZioVersion % "test"

  val pprint            = "com.lihaoyi" %% "pprint"      % PPrintVersion
  val `scala-compat`    = "org.scala-lang.modules" %% "scala-java8-compat" % CompatVersion
  // Get the ability to optionally format code from here. TODO remove this dependency
  val `quill-util`      = "io.getquill" %% "quill-util" % QuillUtilVersion

}