/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.interpreter

import scala.reflect.internal.util.ScalaClassLoader
import scala.tools.util.PathResolver

import java.io.{PrintWriter, StringWriter}

/** Java 8 tool locator. */
object CodePrinterToolLocator {
  /** Locate the tool. */
  def tool(intp: IMain): CodePrinterTool = {
    val loader = PathResolver.SupplementalLocations.platformTools.map(tools =>
      ScalaClassLoader.fromURLs(Seq(tools.toURL), intp.classLoader))
      .getOrElse(intp.classLoader)
    val writer = new StringWriter
    val javapWriter = new PrintWriter(writer, /*autoflush=*/ true) // was intp.reporter.out
    val javap = JavapClass(loader, javapWriter, intp)
    if (javap.JavapTool.isAvailable)
      new CodePrinter {
        def print(line: String) =
          try {
            val res = javap(Seq(line))
            if (res.exists(_.isError)) "There was an error."
            else {
              res.foreach(_.show())
              writer.toString
            }
          } catch {
            case _: NoClassDefFoundError => "Use `scala -nobootcp` to load javap."
          }
      }
    else CodePrinter.NoTool
  }
}
