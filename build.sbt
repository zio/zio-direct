import ReleaseTransformations._
//import com.typesafe.sbt.SbtScalariform.ScalariformKeys
//import scalariform.formatter.preferences._
import com.jsuereth.sbtpgp.PgpKeys.publishSigned
import sbtrelease.ReleasePlugin
import scala.sys.process.Process
import java.io.{File => JFile}
import Dependencies._

addCommandAlias("fmt", "all scalafmt test:scalafmt")

lazy val `zio-direct` =
  (project in file("."))
    .settings(releaseSettings: _*)
    .settings(basicSettings: _*)
    .settings(
      resolvers ++= Seq(
        Resolver.mavenLocal,
        "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
        "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
      ),
      testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
      libraryDependencies ++= Seq(
        pprint,
        zio,
        `quill-util`,
        `zio-test`,
        `zio-test-sbt`
      )
    )

lazy val basicSettings = Seq(
  libraryDependencies += `scala-compat`,
  excludeDependencies ++= Seq(
    // TODO Only if scala3
    ExclusionRule("org.scala-lang.modules", "scala-collection-compat_2.13")
  ),
  scalaVersion := {
    "3.2.0"
  },
  organization := "dev.zio",
  // The -e option is the 'error' report of ScalaTest. We want it to only make a log
  // of the failed tests once all tests are done, the regular -o log shows everything else.
  // Test / testOptions ++= Seq(
  //   Tests.Argument(TestFrameworks.ScalaTest, "-oF")
  //   //  /*, "-eGNCXEHLOPQRM"*/, "-h", "target/html", "-u", "target/junit"
  //   //Tests.Argument(TestFrameworks.ScalaTest, "-u", "junits")
  //   //Tests.Argument(TestFrameworks.ScalaTest, "-h", "testresults")
  // ),
  scalacOptions ++= Seq(
    "-language:implicitConversions", "-explain"
  )
)

lazy val releaseSettings = ReleasePlugin.extraReleaseCommands ++ Seq(
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  pgpSecretRing := file("local.secring.gpg"),
  pgpPublicRing := file("local.pubring.gpg"),
  releaseVersionBump := sbtrelease.Version.Bump.Next,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseProcess := {
    Seq[ReleaseStep]() ++
    doOnDefault(checkSnapshotDependencies) ++
    doOnDefault(inquireVersions) ++
    doOnDefault(runClean) ++
    doOnPush   (setReleaseVersion) ++
    doOnPush   (commitReleaseVersion) ++
    doOnPush   (tagRelease) ++
    doOnDefault(publishArtifacts) ++
    doOnPush   (setNextVersion) ++
    doOnPush   (commitNextVersion) ++
    //doOnPush(releaseStepCommand("sonatypeReleaseAll")) ++
    doOnPush   (pushChanges)
  },
  homepage := Some(url("http://github.com/zio/zio-defer-await")),
  licenses := List(("Apache License 2.0", url("https://raw.githubusercontent.com/getquill/protoquill/master/LICENSE.txt"))),
  developers := List(
    Developer("deusaquilus", "Alexander Ioffe", "", url("https://github.com/deusaquilus"))
  ),
  scmInfo := Some(
    ScmInfo(url("https://github.com/zio/zio-defer-await"), "git@github.com:zio/zio-defer-await.git")
  )
)

def doOnDefault(steps: ReleaseStep*): Seq[ReleaseStep] =
  Seq[ReleaseStep](steps: _*)

def doOnPush(steps: ReleaseStep*): Seq[ReleaseStep] =
  if (skipPush)
    Seq[ReleaseStep]()
  else
    Seq[ReleaseStep](steps: _*)

val skipPush =
  sys.props.getOrElse("skipPush", "false").toBoolean
