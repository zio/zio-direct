import BuildHelper._
import Dependencies._

inThisBuild(
  List(
    organization  := "dev.zio",
    homepage      := Some(url("https://zio.github.io/zio-direct/")),
    licenses      := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers    := List(
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

lazy val root = (project in file("."))
  .aggregate(
    `zio-direct`,
    docs
  )
  .settings(
    crossScalaVersions := Nil,
    publish / skip     := true
  )

lazy val `zio-direct` = project
    .in(file("zio-direct"))
    .settings(stdSettings("zio-direct"))
    .settings(crossProjectSettings)
    .settings(dottySettings)
    .settings(buildInfoSettings("zio.direct"))
    .enablePlugins(BuildInfoPlugin)
    .settings(
      scalaVersion := ScalaDotty,
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



lazy val docs = project
  .in(file("zio-direct-docs"))
  .settings(stdSettings("zio-direct"))
  .settings(macroDefinitionSettings)
  .settings(
    excludeDependencies ++= Seq(
      "com.geirsson" % "metaconfig-core_2.13",
      "com.geirsson" % "metaconfig-typesafe-config_2.13",
      "org.typelevel" % "paiges-core_2.13",
    ),
    crossScalaVersions                         := Seq(),
    scalaVersion                               := ScalaDotty,
    publish / skip                             := true,
    moduleName                                 := "zio-direct-docs",
    scalacOptions -= "-Yno-imports",
    //scalacOptions -= "-Xfatal-warnings",
    //ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(`zio-direct`),
    //ScalaUnidoc / unidoc / target              := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    //cleanFiles += (ScalaUnidoc / unidoc / target).value,
    //docusaurusCreateSite                       := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    //docusaurusPublishGhpages                   := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .dependsOn(`zio-direct`)
  .enablePlugins(MdocPlugin, DocusaurusPlugin) //ScalaUnidocPlugin

addCommandAlias("fmt", "all scalafmt test:scalafmt")
