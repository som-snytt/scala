/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Stephane Micheloud
 * Adapted from Lex Spoon's sbaz manual
 */

package scala.tools.docutil

import java.nio.file.{Files, Path, Paths}
import java.time._, format._, DateTimeFormatter.{BASIC_ISO_DATE => BasicDate}

import scala.tools.nsc.settings.ScalaVersion

class ManMaker(htmlout: Path, manout: Path) {

  def makeMan(version: ScalaVersion, dated: LocalDate, command: String): Unit = {
    val classname = s"scala.man1.$command"

    val htmlFile = htmlout.resolve(s"$command.html")
    val htmlOut  = Files.newOutputStream(htmlFile)
    EmitHtml.emitHtml(classname, version, dated, htmlOut)

    val manFile = manout.resolve("man1").resolve(s"$command.1")
    val manOut  = Files.newOutputStream(manFile)
    EmitManPage.emitManPage(classname, manOut)
  }
}

/** Command line runner for ManMaker which is called from the sbt build. */
object ManMaker {
  import scala.util.Try
  object V {
    def unapply(s: String): Option[ScalaVersion] = Try(ScalaVersion(s)).toOption
  }
  object D {
    def unapply(s: String): Option[LocalDate] = Try(LocalDate.parse(s.take(8), BasicDate)).toOption
  }
  def main(args: Array[String]): Unit = {
    val Array(V(version), D(dated), commands, htmlout, manout) = args
    val mm = new ManMaker(Paths get htmlout, Paths get manout)
    for (command <- commands.split(",").map(_.trim()) if command.nonEmpty)
      mm.makeMan(version, dated, command)
  }
}
