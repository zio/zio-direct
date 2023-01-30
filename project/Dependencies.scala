import sbt.Keys.scalaVersion
import sbt._

object Dependencies {

  implicit class ModuleIdOps(module: ModuleID) {
    def excludeVersionConflicting =
      module.excludeAll(
        (Seq(
          // Remve scala-collection-compat, pprint, fansi, and sourcecode from dependencies here
          // since we are manually importing them
          ExclusionRule(organization = "org.scala-lang.modules"),
          ExclusionRule(organization = "com.lihaoyi")
        )): _*
      )
  }

  val ZioVersion = "2.0.6"
  val PPrintVersion = "0.6.6"
  val FansiVersion = "0.4.0"
  val SourceCodeVersion = "0.2.8"
  val CompatVersion = "1.0.1"
  val CollectionCompatVersion = "2.9.0"
  val QuillUtilVersion = "4.6.0"

  val zio               = "dev.zio" %% "zio"             % ZioVersion
  val `zio-streams`     = "dev.zio" %% "zio-streams"     % ZioVersion
  val `zio-prelude`     = "dev.zio" %% "zio-prelude"     % "1.0.0-RC16"
  val `zio-test`        = "dev.zio" %% "zio-test"        % ZioVersion % "test"
  val `zio-test-sbt`    = "dev.zio" %% "zio-test-sbt"    % ZioVersion % "test"

  val pprint            = ("com.lihaoyi" %% "pprint"      % PPrintVersion).excludeVersionConflicting
  val fansi            = ("com.lihaoyi" %% "fansi"      % FansiVersion).excludeVersionConflicting
  val sourcecode            = ("com.lihaoyi" %% "sourcecode"      % SourceCodeVersion).excludeVersionConflicting

  val `scala-java8-compat`    = "org.scala-lang.modules" %% "scala-java8-compat" % CompatVersion
  val `scala-collection-compat`    = "org.scala-lang.modules" %% "scala-collection-compat" % CollectionCompatVersion
  // Get the ability to optionally format code from here. TODO remove this dependency
  val `quill-util`      = ("io.getquill" %% "quill-util" % QuillUtilVersion).excludeVersionConflicting

}