# LucreEvent

## statement

LucreEvent provides a reactive event and observer layer for LucreSTM, a software transactional memory and persistence library for the Scala programming language. The reactive system implements event graphs which also can be persistent, along with live observers.

LucreEvent is (C)opyright 2011&ndash;2012 by Hanns Holger Rutz. All rights reserved. It is released under the [GNU General Public License](https://raw.github.com/Sciss/LucreEvent/master/licenses/LucreEvent-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

Further reading:

 - Rutz, H.H., "A Reactive, Confluently Persistent Framework for the Design of Computer Music Systems," in Proceedings of the 9th Sound and Music Computing Conference (SMC), Copenhagen 2012.
 - Gasiunas, V. and Satabin, L. and Mezini, M. and Núñez, A. and Noyé, J., "EScala: Modular Event-Driven Object Interactions in Scala," in Proceedings of the tenth international conference on Aspect-oriented software development, pp. 227--240, 2011.

## requirements / installation

LucreEvent builds with sbt 0.12 against Scala 2.9.2. It depends on [LucreSTM](https://github.com/Sciss/LucreSTM) which should be automatically found by sbt.

## linking

The following dependency is necessary:

    "de.sciss" %% "lucreevent" % "1.3.+"

## documentation

At the moment, there are only sparse scaladocs, I'm afraid (run `sbt doc`). The basic concept:

TODO!

 - The system distinguishes between persisted and live references (even with a pure in-memory system), where a change in an observed object is propagated through a mechanism called 'tunnelling', pushing along stub nodes until a live node is found, which in turn evaluates the path in pull fashion.

## creating an IntelliJ IDEA project

To develop the sources of this library, we recommend IntelliJ IDEA. If you haven't globally installed the sbt-idea plugin yet, create the following contents in `~/.sbt/plugins/build.sbt`:

    resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

    addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

Then to create the IDEA project, run `sbt gen-idea`.
