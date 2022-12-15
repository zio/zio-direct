import sbt.Keys._
import sbt._
import sbtbuildinfo.BuildInfoKeys._
import sbtbuildinfo._


object BuildHelper {
  private val versions: Map[String, String] = {
    import org.snakeyaml.engine.v2.api.{Load, LoadSettings}

    import java.util.{List => JList, Map => JMap}
    import scala.jdk.CollectionConverters._

    val doc  = new Load(LoadSettings.builder().build())
      .loadFromReader(scala.io.Source.fromFile(".github/workflows/ci.yaml").bufferedReader())
    val yaml = doc.asInstanceOf[JMap[String, JMap[String, JMap[String, JMap[String, JMap[String, JList[String]]]]]]]
    val list = yaml.get("jobs").get("test").get("strategy").get("matrix").get("scala").asScala
    list.map(v => (v.split('.').take(2).mkString("."), v)).toMap
  }

  val Scala213: String                      = versions("2.13")
  val ScalaDotty: String                    = versions("3.2")

  val SilencerVersion = "1.7.12"

  private val stdOptions = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked"
  ) ++ {
    if (sys.env.contains("CI")) {
      Seq() //"-Xfatal-warnings"
    } else {
      Nil // to enable Scalafix locally
    }
  }

  private val std2xOptions = Seq(
    "-language:higherKinds",
    "-language:existentials",
    "-explaintypes",
    "-Yrangepos",
    "-Xlint:_,-missing-interpolator,-type-parameter-shadow",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  )

  def buildInfoSettings(packageName: String) =
    Seq(
      buildInfoKeys    := Seq[BuildInfoKey](organization, moduleName, name, version, scalaVersion, sbtVersion, isSnapshot),
      buildInfoPackage := packageName
    )

  val dottySettings = Seq(
    crossScalaVersions := Seq(ScalaDotty),
    scalacOptions ++= {
      if (scalaVersion.value == ScalaDotty)
        Seq() // Seq("-noindent", "-Xcheck-macros")
      else
        Seq()
    },
    scalacOptions --= {
      if (scalaVersion.value == ScalaDotty)
        Seq() //Seq("-Xfatal-warnings")
      else
        Seq()
    },
    Compile / doc / sources  := {
      val old = (Compile / doc / sources).value
      if (scalaVersion.value == ScalaDotty) {
        Nil
      } else {
        old
      }
    },
    Test / parallelExecution := {
      val old = (Test / parallelExecution).value
      if (scalaVersion.value == ScalaDotty) {
        false
      } else {
        old
      }
    }
  )

  // Keep this consistent with the version in .core-tests/shared/src/test/scala/REPLSpec.scala
  val replSettings = makeReplSettings {
    """|import zio._
       |import zio.console._
       |import zio.duration._
       |import zio.Runtime.default._
       |implicit class RunSyntax[A](io: ZIO[ZEnv, Any, A]){ def unsafeRun: A = Runtime.default.unsafeRun(io.provideLayer(ZEnv.live)) }
    """.stripMargin
  }

  // Keep this consistent with the version in .streams-tests/shared/src/test/scala/StreamREPLSpec.scala
  val streamReplSettings = makeReplSettings {
    """|import zio._
       |import zio.console._
       |import zio.duration._
       |import zio.stream._
       |import zio.Runtime.default._
       |implicit class RunSyntax[A](io: ZIO[ZEnv, Any, A]){ def unsafeRun: A = Runtime.default.unsafeRun(io.provideLayer(ZEnv.live)) }
    """.stripMargin
  }

  def makeReplSettings(initialCommandsStr: String) = Seq(
    // In the repl most warnings are useless or worse.
    // This is intentionally := as it's more direct to enumerate the few
    // options we do want than to try to subtract off the ones we don't.
    // One of -Ydelambdafy:inline or -Yrepl-class-based must be given to
    // avoid deadlocking on parallel operations, see
    //   https://issues.scala-lang.org/browse/SI-9076
    Compile / console / scalacOptions   := Seq(
      "-Ypartial-unification",
      "-language:higherKinds",
      "-language:existentials",
      "-Yno-adapted-args",
      "-Xsource:2.13",
      "-Yrepl-class-based"
    ),
    Compile / console / initialCommands := initialCommandsStr
  )

  def extraOptions(scalaVersion: String) =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((3, 0))  =>
        Seq(
          // "-language:implicitConversions",
          // "-Xignore-scala2-macros"
        )
      case Some((2, 13)) =>
        Seq(
          // "-Ywarn-unused:params,-implicits"
        ) ++ std2xOptions
      case Some((2, 12)) =>
        Seq(
          // "-opt-warnings",
          // "-Ywarn-extra-implicit",
          // "-Ywarn-unused:_,imports",
          // "-Ywarn-unused:imports",
          // "-Ypartial-unification",
          // "-Yno-adapted-args",
          // "-Ywarn-inaccessible",
          // "-Ywarn-infer-any",
          // "-Ywarn-nullary-override",
          // "-Ywarn-nullary-unit",
          // "-Ywarn-unused:params,-implicits",
          // "-Xfuture",
          "-Xsource:2.13",
          // "-Xmax-classfile-name",
          // "242"
        ) ++ std2xOptions
      case Some((2, 11)) =>
        Seq(
          // "-Ypartial-unification",
          // "-Yno-adapted-args",
          // "-Ywarn-inaccessible",
          // "-Ywarn-infer-any",
          // "-Ywarn-nullary-override",
          // "-Ywarn-nullary-unit",
          // "-Xexperimental",
          // "-Ywarn-unused-import",
          // "-Xfuture",
          "-Xsource:2.13",
          // "-Xmax-classfile-name",
          // "242"
        ) ++ std2xOptions
      case _             => Seq.empty
    }

  def platformSpecificSources(conf: String, baseDirectory: File)(versions: String*) = for {
    version  <- "scala" :: versions.toList.map("scala-" + _)
    result    = baseDirectory / "src" / conf / version
    //_ = {println(s"------- Write to directory: ${result}")}
    if result.exists
  } yield result

  def crossPlatformSources(scalaVer: String, conf: String, baseDir: File) = {
    val versions = CrossVersion.partialVersion(scalaVer) match {
      case Some((2, 11)) =>
        List("2.11", "2.11+", "2.11-2.12", "2.x")
      case Some((2, 12)) =>
        List("2.12", "2.11+", "2.12+", "2.11-2.12", "2.12-2.13", "2.x")
      case Some((2, 13)) =>
        List("2.13", "2.11+", "2.12+", "2.13+", "2.12-2.13", "2.x")
      case Some((3, _))  =>
        List("dotty", "2.11+", "2.12+", "2.13+", "3.x")
      case _             =>
        List()
    }
    platformSpecificSources(conf, baseDir)(versions: _*)
  }

  lazy val crossProjectSettings = Seq(
    crossScalaVersions := Seq(Scala213, ScalaDotty),
    Compile / unmanagedSourceDirectories ++= {
      crossPlatformSources(
        scalaVersion.value,
        "main",
        baseDirectory.value
      )
    },
    Test / unmanagedSourceDirectories ++= {
      crossPlatformSources(
        scalaVersion.value,
        "test",
        baseDirectory.value
      )
    }
  )

  def stdSettings(prjName: String) = Seq(
    name                                   := s"$prjName",
    ThisBuild / scalaVersion               := Scala213,
    scalacOptions                          := stdOptions ++ extraOptions(scalaVersion.value),
  )

  def macroDefinitionSettings = Seq(
    scalacOptions += "-language:experimental.macros",
    libraryDependencies ++= {
      if (scalaVersion.value == ScalaDotty) Seq()
      else
        Seq(
          "org.scala-lang" % "scala-reflect"  % scalaVersion.value % "provided",
          "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
        )
    }
  )

  def welcomeMessage = onLoadMessage := {
    import scala.Console

    def header(text: String): String = s"${Console.RED}$text${Console.RESET}"

    def item(text: String): String    = s"${Console.GREEN}> ${Console.CYAN}$text${Console.RESET}"
    def subItem(text: String): String = s"  ${Console.YELLOW}> ${Console.CYAN}$text${Console.RESET}"

    s"""|${header(" ________ ___")}
        |${header("|__  /_ _/ _ \\")}
        |${header("  / / | | | | |")}
        |${header(" / /_ | | |_| |")}
        |${header(s"/____|___\\___/   ${version.value}")}
        |
        |Useful sbt tasks:
        |${item("build")} - Prepares sources, compiles and runs tests.
        |${item("prepare")} - Prepares sources by applying both scalafix and scalafmt
        |${item("fix")} - Fixes sources files using scalafix
        |${item("fmt")} - Formats source files using scalafmt
        |${item("~compileJVM")} - Compiles all JVM modules (file-watch enabled)
        |${item("testJVM")} - Runs all JVM tests
        |${item("testOnly *.YourSpec -- -t \"YourLabel\"")} - Only runs tests with matching term e.g.
        |${subItem("coreTestsJVM/testOnly *.ZIOSpec -- -t \"happy-path\"")}
        |${item("docs/docusaurusCreateSite")} - Generates the ZIO microsite
      """.stripMargin
  }

  implicit class ModuleHelper(p: Project) {
    def module: Project = p.in(file(p.id)).settings(stdSettings(p.id))
  }
}