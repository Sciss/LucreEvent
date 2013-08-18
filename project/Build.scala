import sbt._
import Keys._
import sbtbuildinfo.Plugin._

object Build extends sbt.Build {
  lazy val stmVersion  = "2.0.+"
  lazy val dataVersion = "2.2.+"

  lazy val root: Project = Project(
    id            = "lucreevent",
    base          = file("."),
    aggregate     = Seq(core, expr),
    dependencies  = Seq(core, expr), // i.e. root = full sub project. if you depend on root, will draw all sub modules.
    settings      = Project.defaultSettings ++ Seq(
      publishArtifact in (Compile, packageBin) := false, // there are no binaries
      publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
      publishArtifact in (Compile, packageSrc) := false  // there are no sources
    )
  )

  // convert the base version to a compatible version for
  // library dependencies. e.g. `"1.3.1"` -> `"1.3.+"`
  object Compatible {
    def unapply(v: String) = {
      val c = if(v endsWith "SNAPSHOT") v else {
        require(v.count(_ == '.') == 2)
        val i = v.lastIndexOf('.') + 1
        v.substring(0, i) + "+"
      }
      Some(c)
    }
  }

  lazy val core = Project(
    id        = "lucreevent-core",
    base      = file("core"),
    settings  = Project.defaultSettings ++ buildInfoSettings ++ Seq(
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
    settings      = Project.defaultSettings ++ Seq(
      libraryDependencies ++= Seq(
        "de.sciss" %% "lucredata-core" % dataVersion,
        "de.sciss" %% "lucrestm-bdb"   % stmVersion % "test",
        "org.scalatest" %% "scalatest" % "1.9.1" % "test"
      )
    )
  )
}
