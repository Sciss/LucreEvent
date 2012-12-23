# LucreExpr

## statement

LucreExpr is a library for the Scala programming language which provides an expression tree system on top of LucreSTM's software transactional memory and reactive event system.

LucreExpr is (C)opyright 2011&ndash;2012 by Hanns Holger Rutz. All rights reserved. It is released under the [GNU General Public License](https://raw.github.com/Sciss/LucreExpr/master/licenses/LucreExpr-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

## building

LucreSTM builds with sbt 0.12 against Scala 2.9.2. It depends on [LucreData-Core](http:/github.com/Sciss/LucreData) which should be automatically retrieved from Maven Central.

## linking

The following dependency is necessary:

    resolvers += "Oracle Repository" at "http://download.oracle.com/maven"
    
    "de.sciss" %% "lucreexpr" % "1.4.+"

## creating an IntelliJ IDEA project

To develop the sources of LucreExpr, we recommend IntelliJ IDEA. If you haven't globally installed the sbt-idea plugin yet, create the following contents in `~/.sbt/plugins/build.sbt`:

    resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

    addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

Then to create the IDEA project, run `sbt gen-idea`.
