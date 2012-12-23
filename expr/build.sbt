name := "LucreExpr"

version := "1.6.0"

organization := "de.sciss"

description := "Reactive Expression Trees based on LucreSTM"

homepage := Some( url( "https://github.com/Sciss/LucreExpr" ))

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

scalaVersion := "2.10.0"

crossScalaVersions := Seq( "2.10.0", "2.9.2" )

libraryDependencies ++= Seq(
   "de.sciss" %% "lucredata-core" % "1.6.+",
   "de.sciss" %% "lucreevent" % "1.6.+"
)

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" ) // , "-Xelide-below", "INFO" ) // elide debug logging!

testOptions in Test += Tests.Argument( "-oDF" )   // ScalaTest: durations and full stack traces

parallelExecution in Test := false

// publishArtifact in (Compile, packageDoc) := false   // disable doc generation during development cycles

// ---- build info ----

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq( name, organization, version, scalaVersion, description,
   BuildInfoKey.map( homepage ) { case (k, opt) => k -> opt.get },
   BuildInfoKey.map( licenses ) { case (_, Seq( (lic, _) )) => "license" -> lic }
)

buildInfoPackage := "de.sciss.lucre.expr"

// ---- publishing ----

publishMavenStyle := true

publishTo <<= version { (v: String) =>
   Some( if( v.endsWith( "-SNAPSHOT" ))
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
   else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
   )
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra :=
<scm>
  <url>git@github.com:Sciss/LucreExpr.git</url>
  <connection>scm:git:git@github.com:Sciss/LucreExpr.git</connection>
</scm>
<developers>
   <developer>
      <id>sciss</id>
      <name>Hanns Holger Rutz</name>
      <url>http://www.sciss.de</url>
   </developer>
</developers>

// ---- ls.implicit.ly ----

seq( lsSettings :_* )

(LsKeys.tags in LsKeys.lsync) := Seq( "stm", "software-transactional-memory", "reactive", "event", "expression" )

(LsKeys.ghUser in LsKeys.lsync) := Some( "Sciss" )

(LsKeys.ghRepo in LsKeys.lsync) := Some( "LucreExpr" )

// bug in ls -- doesn't find the licenses from global scope
(licenses in LsKeys.lsync) := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))
