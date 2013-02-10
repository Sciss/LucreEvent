import sbt._
import Keys._
import sbtbuildinfo.Plugin._

object Build extends sbt.Build {
   lazy val audiowidgets: Project = Project(
      id        = "lucreevent",
      base      = file( "." ),
      aggregate = Seq( core, expr )
   )

   lazy val core = Project(
      id        = "lucreevent-core",
      base      = file( "core" ),
      settings     = Project.defaultSettings ++ buildInfoSettings ++ Seq(
         libraryDependencies ++= Seq(
            "de.sciss" %% "lucrestm-core" % "1.7.+"
         ),
         sourceGenerators in Compile <+= buildInfo,
         buildInfoKeys := Seq( name, organization, version, scalaVersion, description,
            BuildInfoKey.map( homepage ) { case (k, opt) => k -> opt.get },
            BuildInfoKey.map( licenses ) { case (_, Seq( (lic, _) )) => "license" -> lic }
         ),
         buildInfoPackage := "de.sciss.lucre.event"
      )
   )

   lazy val expr = Project(
      id           = "lucreevent-expr",
      base         = file( "expr" ),
      dependencies = Seq( core ),
      settings     = Project.defaultSettings ++ Seq(
         libraryDependencies ++= Seq(
            "de.sciss" %% "lucredata-core" % "1.7.+",
            "de.sciss" %% "lucrestm-bdb" % "1.7.+" % "test"
         )
      )
   )
}
