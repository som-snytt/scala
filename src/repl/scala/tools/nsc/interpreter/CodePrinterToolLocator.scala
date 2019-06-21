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
  def tool(intp: IMain): Option[CodePrinterTool] = {
    val loader = PathResolver.SupplementalLocations.platformTools.map(tools =>
      ScalaClassLoader.fromURLs(Seq(tools.toURL), intp.classLoader))
      .getOrElse(intp.classLoader)
    // Since the tool is loaded by reflection, it might not be available
    def TaskClass = loader.tryToInitializeClass[Task](JavapTask)
    TaskClass.map(_ => new CodePrinterTool {
      def print(out: PrintWriter, err: PrintWriter, args: Array[String]): Unit = out.println("OUTPUT")
    })
/*
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
*/
  }

  // Java 7 and up
  val JavapTask = "com.sun.tools.javap.JavapTask"

  private type Task = {
    def call(): Boolean                             // true = ok
    //def run(args: Array[String]): Int             // all args
    //def handleOptions(args: Array[String]): Unit  // options, then run() or call()
  }
  // result of Task.run
  //object TaskResult extends Enumeration {
  //  val Ok, Error, CmdErr, SysErr, Abnormal = Value
  //}
/*
  def TaskCtor  = TaskClass.map(_.getConstructor(
    classOf[Writer],
    classOf[JavaFileManager],
    classOf[DiagnosticListener[_]],
    classOf[JIterable[String]],
    classOf[JIterable[String]]
  ))
  class JavaReporter extends DiagnosticListener[JavaFileObject] with Clearable {
    type JDiagnostic = Diagnostic[_ <: JavaFileObject]
    val diagnostics = new ConcurrentLinkedQueue[JDiagnostic]
    override def report(d: JDiagnostic): Unit = diagnostics.add(d)
    override def clear() = diagnostics.clear()
    /** All diagnostic messages.
     *  @param locale Locale for diagnostic messages, null by default.
     */
    def messages(implicit locale: Locale = null) = diagnostics.asScala.map(_.getMessage(locale)).toList

    def reportable(): String = {
      clear()
      if (messages.nonEmpty) messages.mkString("", EOL, EOL) else ""
    }
  }
  // DisassemblerTool.getStandardFileManager(reporter,locale,charset)
  def defaultFileManager: JavaFileManager =
    loader.tryToLoadClass[JavaFileManager]("com.sun.tools.javap.JavapFileManager").map(_.getMethod(
      "create",
      classOf[DiagnosticListener[_]],
      classOf[PrintWriter]
    )).
  invoke (null, reporter, new PrintWriter(System.err, true))).asInstanceOf[JavaFileManager] orFailed null
  */
}

/*

    val reporter = new JavaReporter


    // manages named arrays of bytes, which might have failed to load
    class JavapFileManager(val managed: Seq[Input])(delegate: JavaFileManager = defaultFileManager)
*/
