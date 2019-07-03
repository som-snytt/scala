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

import scala.collection.mutable.Clearable
import scala.jdk.CollectionConverters._
import scala.util.chaining._
import scala.util.Try

import java.io.{PrintWriter, StringWriter, Writer}
import java.lang.{Iterable => JIterable}
import java.util.Locale
import java.util.concurrent.ConcurrentLinkedQueue
import javax.tools._

/** Java 8 tool locator. */
object CodePrinterToolLocator {

  // pair of target (class) name and its data if found
  private type Input = Tuple2[String, Try[Array[Byte]]]

  // Java 7 and up
  private val JavapTask = "com.sun.tools.javap.JavapTask"

  private type Task = {
    def call(): Boolean                             // true = ok
    //def run(args: Array[String]): Int             // all args
    //def handleOptions(args: Array[String]): Unit  // options, then run() or call()
  }
  // result of Task.run
  //object TaskResult extends Enumeration {
  //  val Ok, Error, CmdErr, SysErr, Abnormal = Value
  //}

  /** Locate the tool. */
  def tool(intp: IMain): Option[CodePrinterTool] = {
    val loader = PathResolver.SupplementalLocations.platformTools.map(tools =>
      ScalaClassLoader.fromURLs(Seq(tools.toURL), intp.classLoader))
      .getOrElse(intp.classLoader)
    // Since the tool is loaded by reflection, it might not be available
    def TaskClass = loader.tryToInitializeClass[Task](JavapTask)
    def TaskCtor  = TaskClass.map(_.getConstructor(
      classOf[Writer],
      classOf[JavaFileManager],
      classOf[DiagnosticListener[_]],
      classOf[JIterable[String]],
      classOf[JIterable[String]]
    ))
    val reporter = new JavaReporter

    // DisassemblerTool.getStandardFileManager(reporter,locale,charset)
    def defaultFileManager: Option[JavaFileManager] =
      loader.tryToLoadClass[JavaFileManager]("com.sun.tools.javap.JavapFileManager").map(_.getMethod(
          "create",
          classOf[DiagnosticListener[_]],
          classOf[PrintWriter]
        ).invoke(null, reporter, new PrintWriter(System.err, true)).asInstanceOf[JavaFileManager])
  
    // eventually, use the tool interface
    //ServiceLoader.load(classOf[javax.tools.DisassemblerTool]).
    //getTask(writer, fileManager, reporter, options.asJava, classes.asJava)
    TaskCtor.map(ctor => new CodePrinterTool {
      def print(out: PrintWriter, err: PrintWriter, args: Array[String]): Unit = {
        val options: Seq[String] = Nil
        val classes: Seq[String] = Nil
        val inputs:  Seq[Input]  = Nil
        val toolopts = options.filter(_ != "-filter")
        val fileManager = new JavapFileManager(inputs)(defaultFileManager.getOrElse(null))
        val task = ctor.newInstance(out, fileManager, reporter, toolopts.asJava, classes.asJava)
        if (!task.call()) err.println("Error running javap task.")
      }
    })
  }
  // Collects diagnostics.
  class JavaReporter extends DiagnosticListener[JavaFileObject] with Clearable {
    type JDiagnostic = Diagnostic[_ <: JavaFileObject]
    val diagnostics = new ConcurrentLinkedQueue[JDiagnostic]
    override def report(d: JDiagnostic): Unit = diagnostics.add(d)
    override def clear() = diagnostics.clear()
    /** All diagnostic messages.
     *  @param locale Locale for diagnostic messages, null by default.
     */
    def messages(implicit locale: Locale = null) = diagnostics.asScala.map(_.getMessage(locale)).toList

    def reportable(): String = messages.mkLines.tap(_ => clear())
  }
  // manages named arrays of bytes, which might have failed to load
  class JavapFileManager(val managed: Seq[Input])(delegate: JavaFileManager)
      extends ForwardingJavaFileManager[JavaFileManager](delegate) {
    import JavaFileManager.Location
    import JavaFileObject.Kind, Kind._, StandardLocation._
    import java.io.{ByteArrayInputStream, InputStream}
    import java.net.{URI, URISyntaxException}

    // name#fragment is OK, but otherwise fragile
    def uri(name: String): URI =
      try new URI(name) // new URI("jfo:" + name)
      catch { case _: URISyntaxException => new URI("dummy") }

    def inputNamed(name: String): Try[Array[Byte]] = (managed find (_._1 == name)).get._2
    def managedFile(name: String, kind: Kind) = kind match {
      case CLASS  => fileObjectForInput(name, inputNamed(name), kind)
      case _      => null
    }
    // todo: just wrap it as scala abstractfile and adapt it uniformly
    def fileObjectForInput(name: String, bytes: Try[Array[Byte]], kind: Kind): JavaFileObject =
      new SimpleJavaFileObject(uri(name), kind) {
        override def openInputStream(): InputStream = new ByteArrayInputStream(bytes.get)
        // if non-null, ClassWriter wrongly requires scheme non-null
        override def toUri: URI = null
        override def getName: String = name
        // suppress
        override def getLastModified: Long = -1L
      }
    override def getJavaFileForInput(location: Location, className: String, kind: Kind): JavaFileObject =
      location match {
        case CLASS_PATH => managedFile(className, kind)
        case _          => null
      }
    override def hasLocation(location: Location): Boolean =
      location match {
        case CLASS_PATH => true
        case _          => false
      }
  }

  private implicit class MkLines[A](private val ss: IterableOnce[String]) extends AnyVal {
    import java.lang.System.lineSeparator
    def mkLines: String = {
      val it = ss.iterator
      if (it.hasNext) it.mkString("", lineSeparator, lineSeparator) else ""
    }
  }
}
