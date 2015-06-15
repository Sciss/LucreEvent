lazy val baseName       = "LucreEvent"
lazy val baseNameL      = baseName.toLowerCase
lazy val projectVersion = "2.7.4"

// ---- base settings ----

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  description        := "Reactive event-system for LucreSTM",
  homepage           := Some(url(s"https://github.com/Sciss/$baseName")),
  licenses           := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
  scalaVersion       := "2.11.6",
  crossScalaVersions := Seq("2.11.6", "2.10.4"),
  resolvers          += "Oracle Repository" at "http://download.oracle.com/maven",  // required for sleepycat
  // retrieveManaged := true,
  scalacOptions     ++= Seq(
    // "-no-specialization",    // fuck yeah, cannot use this option because of SI-7481 which will be fixed in 2019
    // "-Xelide-below", "INFO", // elide debug logging!
    "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture"
  ),
  // API docs:
  scalacOptions in (Compile, doc) ++= Seq(
    // "-doc-title", name.value,
    // "-diagrams-dot-path", "/usr/local/bin/dot",
    // FUCKING NPE: "-diagrams"
    // "-diagrams-dot-timeout", "20", "-diagrams-debug",
  ),
  testOptions in Test += Tests.Argument("-oDF"),   // ScalaTest: durations and full stack traces
  parallelExecution in Test := false
) ++ publishSettings

// ---- dependencies ----

lazy val stmVersion       = "2.1.2"
lazy val dataVersion      = "2.3.2"
lazy val modelVersion     = "0.3.2"
lazy val scalaTestVersion = "2.2.5"

// ---- projects ----

lazy val root = Project(id = baseNameL, base = file(".")).
  aggregate(core, expr, artifact).
  dependsOn(core, expr, artifact). // i.e. root = full sub project. if you depend on root, will draw all sub modules.
  settings(commonSettings).
  settings(
    publishArtifact in (Compile, packageBin) := false, // there are no binaries
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false  // there are no sources
  )

lazy val core = Project(id = s"$baseNameL-core", base = file("core")).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings).
  settings(
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucrestm-core" % stmVersion
    ),
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) {
        case (k, opt) => k -> opt.get
      },
      BuildInfoKey.map(licenses) {
        case (_, Seq((lic, _))) => "license" -> lic
      }
    ),
    buildInfoPackage := "de.sciss.lucre.event"
  )

lazy val expr = Project(id = s"$baseNameL-expr", base = file("expr")).
  dependsOn(core).
  settings(commonSettings).
  settings(
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucredata-core" % dataVersion,
      "de.sciss" %% "model"          % modelVersion,
      "de.sciss" %% "lucrestm-bdb"   % stmVersion % "test",
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
    )
  )

lazy val artifact = Project(id = s"$baseNameL-artifact", base = file("artifact")).
  dependsOn(expr).
  settings(commonSettings)

// ---- publishing ----

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishTo := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := {
<scm>
  <url>git@github.com:Sciss/{baseName}.git</url>
  <connection>scm:git:git@github.com:Sciss/{baseName}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
  }
)

// ---- ls.implicit.ly ----

// seq(lsSettings :_*)
// (LsKeys.tags   in LsKeys.lsync) := Seq("stm", "software-transactional-memory", "reactive", "event", "expression")
// (LsKeys.ghUser in LsKeys.lsync) := Some("Sciss")
// (LsKeys.ghRepo in LsKeys.lsync) := Some(name.value)
