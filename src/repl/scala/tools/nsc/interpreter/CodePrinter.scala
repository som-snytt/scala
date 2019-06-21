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

// Copyright 2005-2017 LAMP/EPFL and Lightbend, Inc.

package scala.tools.nsc.interpreter

import java.io.PrintWriter
import java.io.StringWriter

/** The underlying tool, either ToolProvider or wrapper for old Task. */
trait CodePrinterTool {
  /** Output from the tool run with the given args is written to out. */
  def print(out: PrintWriter, err: PrintWriter, args: Array[String]): Unit
}

trait CodePrinter {
  def print(target: String): String
}

class DelegatingCodePrinter(printer: CodePrinterTool) extends CodePrinter {
  def print(target: String): String = {

    val writer = new StringWriter
    val javapWriter = new PrintWriter(writer, /*autoflush=*/ true) // was intp.reporter.out
    try {
      printer.print(javapWriter, javapWriter, Array.empty)
      writer.toString
    } finally javapWriter.close()
  }
}

object CodePrinter {
  /** Locate the tool. */
  def tool(intp: IMain): Option[CodePrinterTool] = CodePrinterToolLocator.tool(intp)

  def apply(printer: CodePrinterTool): CodePrinter = new DelegatingCodePrinter(printer)

  object NoTool extends CodePrinter {
    def print(target: String) = "No tool is available for printing bytecode."
  }
}
