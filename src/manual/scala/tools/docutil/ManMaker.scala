/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Stephane Micheloud
 * Adapted from Lex Spoon's sbaz manual
 */

package scala.tools.docutil

import java.nio.file.{Files, Path, Paths}

class ManMaker(htmlout: Path, manout: Path) {

  def makeMan(command: String): Unit = {
    val classname = s"scala.man1.$command"

    val htmlFile = htmlout.resolve(s"$command.html")
    val htmlOut  = Files.newOutputStream(htmlFile)
    EmitHtml.emitHtml(classname, htmlOut)

    val manFile = manout.resolve("man1").resolve(s"$command.1")
    val manOut  = Files.newOutputStream(manFile)
    EmitManPage.emitManPage(classname, manOut)
  }
}

/** Command line runner for ManMaker which is called from the sbt build. */
object ManMaker {
  def main(args: Array[String]): Unit = {
    val Array(commands, htmlout, manout) = args
    val mm = new ManMaker(Paths get htmlout, Paths get manout)
    for (command <- commands.split(",").map(_.trim()) if command.nonEmpty)
      mm.makeMan(command)
  }
}
