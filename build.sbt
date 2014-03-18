name := "LucreEvent"

// ---- base settings ----

lazy val commonSettings = Project.defaultSettings ++ Seq(
  version         := "2.6.0",
  organization    := "de.sciss",
  description     := "Reactive event-system for LucreSTM",
  homepage        := Some(url("https://github.com/Sciss/" + name.value)),
  licenses        := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt")),
  scalaVersion    := "2.10.3",
  resolvers       += "Oracle Repository" at "http://download.oracle.com/maven",  // required for sleepycat
  // retrieveManaged := true,
  scalacOptions  ++= Seq(
    // "-no-specialization",    // fuck yeah, cannot use this option because of SI-7481 which will be fixed in 2019
    // "-Xelide-below", "INFO", // elide debug logging!
    "-deprecation", "-unchecked", "-feature"
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

lazy val stmVersion    = "2.0.2+"

lazy val dataVersion   = "2.2.2+"

lazy val modelVersion  = "0.3.1+"

// ---- projects ----

lazy val root: Project = Project(
  id            = "lucreevent",
  base          = file("."),
  aggregate     = Seq(core, expr),
  dependencies  = Seq(core, expr), // i.e. root = full sub project. if you depend on root, will draw all sub modules.
  settings      = commonSettings ++ Seq(
    publishArtifact in (Compile, packageBin) := false, // there are no binaries
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false  // there are no sources
  )
)

lazy val core = Project(
  id        = "lucreevent-core",
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
  id            = "lucreevent-expr",
  base          = file("expr"),
  dependencies  = Seq(core),
  settings      = commonSettings ++ Seq(
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucredata-core" % dataVersion,
      "de.sciss" %% "model"          % modelVersion,
      "de.sciss" %% "lucrestm-bdb"   % stmVersion % "test",
      "org.scalatest" %% "scalatest" % "2.0" % "test"
    )
  )
)

// ---- publishing ----

publishMavenStyle in ThisBuild := true

publishTo in ThisBuild :=
  Some(if (version.value endsWith "-SNAPSHOT")
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
