def baseName = "LucreEvent"

def projectVersion = "2.7.2-SNAPSHOT"

def baseNameL = baseName.toLowerCase

name := baseName

// ---- base settings ----

lazy val commonSettings = Project.defaultSettings ++ Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  description        := "Reactive event-system for LucreSTM",
  homepage           := Some(url(s"https://github.com/Sciss/$baseName")),
  licenses           := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
  scalaVersion       := "2.11.4",
  crossScalaVersions := Seq("2.11.4", "2.10.4"),
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
)

// ---- dependencies ----

lazy val stmVersion       = "2.1.1"

lazy val dataVersion      = "2.3.0"

lazy val modelVersion     = "0.3.2"

lazy val scalaTestVersion = "2.2.2"

// ---- projects ----

lazy val root: Project = Project(
  id            = baseNameL,
  base          = file("."),
  aggregate     = Seq(core, expr, artifact),
  dependencies  = Seq(core, expr, artifact), // i.e. root = full sub project. if you depend on root, will draw all sub modules.
  settings      = commonSettings ++ Seq(
    publishArtifact in (Compile, packageBin) := false, // there are no binaries
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false  // there are no sources
  )
)

lazy val core = Project(
  id        = s"$baseNameL-core",
  base      = file("core"),
  settings  = commonSettings ++ buildInfoSettings ++ Seq(
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucrestm-core" % stmVersion
    ),
    sourceGenerators in Compile <+= buildInfo,
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
)

lazy val expr = Project(
  id            = s"$baseNameL-expr",
  base          = file("expr"),
  dependencies  = Seq(core),
  settings      = commonSettings ++ Seq(
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucredata-core" % dataVersion,
      "de.sciss" %% "model"          % modelVersion,
      "de.sciss" %% "lucrestm-bdb"   % stmVersion % "test",
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
    )
  )
)

lazy val artifact = Project(
  id            = s"$baseNameL-artifact",
  base          = file("artifact"),
  dependencies  = Seq(expr),
  settings      = commonSettings
)

// ---- publishing ----

publishMavenStyle in ThisBuild := true

publishTo in ThisBuild :=
  Some(if (isSnapshot.value)
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )

publishArtifact in Test := false

pomIncludeRepository in ThisBuild := { _ => false }

pomExtra in ThisBuild := { val n = name.value
<scm>
  <url>git@github.com:Sciss/{n}.git</url>
  <connection>scm:git:git@github.com:Sciss/{n}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
}

// ---- ls.implicit.ly ----

seq(lsSettings :_*)

(LsKeys.tags   in LsKeys.lsync) := Seq("stm", "software-transactional-memory", "reactive", "event", "expression")

(LsKeys.ghUser in LsKeys.lsync) := Some("Sciss")

(LsKeys.ghRepo in LsKeys.lsync) := Some(name.value)
