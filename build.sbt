import BuildHelper._
import Dependencies._

inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://zio.dev/zio-direct")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "deusaquilus",
        "Alexander Ioffe",
        "deusaquilus@gmail.com",
        url("https://github.com/deusaquilus")
      )
    ),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc")
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll; compile:scalafix --check; test:scalafix --check")

addCommandAlias(
  "testJVM",
  ";zio-direct/test"
)

lazy val modules =
  Seq[sbt.ClasspathDep[sbt.ProjectReference]](
    `zio-direct`,
    `zio-direct-test`,
    `zio-direct-streams`,
    `zio-direct-pure`
  ) ++ {
    if (isScala3) Seq[sbt.ClasspathDep[sbt.ProjectReference]](docs) else Seq[sbt.ClasspathDep[sbt.ProjectReference]]()
  }

lazy val root = (project in file("."))
  .aggregate(modules.map(_.project): _*)
  .settings(
    publish / skip := true,
    scalaVersion := `zd.scala.version`
  )
  .aggregate(`zio-direct`, `zio-direct-test`, docs)

lazy val `zio-direct` = project
  .in(file("zio-direct"))
  .settings(stdSettings("zio-direct"))
  .settings(projectModuleSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    crossScalaVersions := Seq(Scala212, Scala213, ScalaDotty),
    Compile / resourceGenerators += Def.task {
      val rootFolder = (Compile / resourceManaged).value / "META-INF"
      rootFolder.mkdirs()
      val compatFile = rootFolder / "intellij-compat.json"
      val compatFileContent = s"""{ "artifact": "${(ThisBuild / organization).value} % zio-direct-intellij_2.13 % ${version.value}" }"""

      val doWrite =
        if (compatFile.exists()) {
          val currentContent = IO.read(compatFile)
          currentContent != compatFileContent
        } else
          true

      if (doWrite) {
        println(s"--- Writing compat file: ${compatFile} - ${compatFileContent} ---")
        IO.write(compatFile, compatFileContent)
      }

      Seq(compatFile)
    }
  )

lazy val `zio-direct-test` = project
  .in(file("zio-direct-test"))
  .settings(stdSettings("zio-direct-test"))
  .settings(buildInfoSettings("zio.direct.test"))
  .settings(projectModuleSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    crossScalaVersions := Seq(Scala212, Scala213, ScalaDotty),
    publish / skip := true
  )
  .dependsOn(`zio-direct` % "compile->compile;test->test")

lazy val `zio-direct-streams` = project
  .in(file("zio-direct-streams"))
  .settings(stdSettings("zio-direct-streams"))
  .settings(buildInfoSettings("zio.direct.streams"))
  .settings(projectModuleSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    crossScalaVersions := Seq(ScalaDotty),
    libraryDependencies ++= Seq(
      `zio-streams`
    )
  )
  .dependsOn(`zio-direct` % "compile->compile;test->test")
  .dependsOn(`zio-direct-test` % "test->test")

lazy val `zio-direct-pure` = project
  .in(file("zio-direct-pure"))
  .settings(stdSettings("zio-direct-pure"))
  .settings(buildInfoSettings("zio.direct.pure"))
  .settings(projectModuleSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    crossScalaVersions := Seq(ScalaDotty),
    libraryDependencies ++= Seq(
      `zio-prelude`
    )
  )
  .dependsOn(`zio-direct` % "compile->compile;test->test")
  .dependsOn(`zio-direct-test` % "test->test")

lazy val docs = project
  .in(file("zio-direct-docs"))
  .settings(stdSettings("zio-direct-docs"))
  .settings(macroDefinitionSettings)
  .settings(
    excludeDependencies ++= Seq(
      ("org.scala-lang.modules" % "scala-collection-compat_2.13"),
      ("com.lihaoyi" % "pprint_2.13"),
      ("com.lihaoyi" % "fansi_2.13"),
      ("com.lihaoyi" % "sourcecode_2.13"),
      ("com.geirsson" % "metaconfig-core_2.13"),
      ("com.geirsson" % "metaconfig-typesafe-config_2.13"),
      ("org.typelevel" % "paiges-core_2.13")
    ),
    crossScalaVersions := Seq(Scala212, Scala213, ScalaDotty),
    scalaVersion := ScalaDotty,
    moduleName := "zio-direct-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    projectName := "ZIO Direct Style",
    mainModuleName := (`zio-direct` / moduleName).value,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(`zio-direct`),
    projectStage := ProjectStage.Development,
    docsPublishBranch := "main"
  )
  .dependsOn(`zio-direct`)
  .enablePlugins(WebsitePlugin)

addCommandAlias("fmt", "all scalafmt test:scalafmt")
