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

trait CodePrinter {
  def print(target: String): String
}

object CodePrinter {
  /** Locate the tool. */
  def tool(intp: IMain): CodePrinter = CodePrinterToolLocator.tool(intp)

  object NoTool extends CodePrinter {
    def print(target: String) = "No tool is available for printing bytecode."
  }
}
