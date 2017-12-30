/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Stephane Micheloud
 * Adapted from Lex Spoon's sbaz manual
 */

package scala.tools.docutil

import java.io.{File, FileOutputStream}

class ManMaker(htmlout: File, manout: File) {

  def makeMan(command: String): Unit = {
    val classname = "scala.man1."+ command

    val htmlFileName = htmlout.getPath + File.separator +
                       command + ".html"
    val htmlFile = new java.io.FileOutputStream(htmlFileName)
    EmitHtml.emitHtml(classname, htmlFile)

    val manFileName = manout.getPath + File.separator +
                      "man1" + File.separator + command + ".1"
    val manFile = new FileOutputStream(manFileName)
    EmitManPage.emitManPage(classname, manFile)
  }
}

/** Command line runner for ManMaker which is called from the sbt build. */
object ManMaker {
  def main(args: Array[String]): Unit = {
    val Array(commands, htmlout, manout) = args
    val mm = new ManMaker(new File(htmlout), new File(manout))
    for (command <- commands.split(",").map(_.trim()) if command.nonEmpty)
      mm.makeMan(command)
  }
}
