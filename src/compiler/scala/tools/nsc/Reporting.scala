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

package scala
package tools
package nsc

import scala.collection.mutable
import scala.reflect.internal.util.StringOps.countElementsAsString
import nsc.settings.{NoScalaVersion, ScalaVersion}

/** Provides delegates to the reporter doing the actual work.
 * PerRunReporting implements per-Run stateful info tracking and reporting
 *
 * TODO: make reporting configurable
 */
trait Reporting extends scala.reflect.internal.Reporting { self: ast.Positions with CompilationUnits with scala.reflect.internal.Symbols =>
  def settings: Settings

  // not deprecated yet, but a method called "error" imported into
  // nearly every trait really must go.  For now using globalError.
  def error(msg: String) = globalError(msg)

  object policy extends Enumeration {
    val Silent, Info, Warn, Error = Value
    case class Philter(pkg: String, esc: Value, isInfractor: Boolean, label: String, version: ScalaVersion) {
      def matches(sym: Symbol, since: String, infractor: String): Boolean = {
        val v = ScalaVersion(since)
        version < v
      }
    }
    val NoPhilter = Philter("", Silent, false, "", NoScalaVersion)
    val config: List[Philter] = {
      def parse(s: String) = {
        if (s.startsWith("since<")) {
          val v = ScalaVersion(s.substring("since<".length), x => reporter.error(NoPosition, s"bad version '$x'"))
          Philter("", Warn, false, "", v)
        } else {
          reporter.error(NoPosition, s"bad policy '$s'")
          NoPhilter
        }
      }
      settings.deprecationPolicy.value.map(parse)
    }
    def apply(pos: Position, sym: Symbol, msg: String, since: String, infractor: String): Value = {
      println(s"checking ${sym.fullName}")
      if (sym.fullName == "scala.tools.nsc.reporters.Reporter") Silent
      else config.find(_.matches(sym, since, infractor)).map(_.esc).getOrElse(Warn)
    }
  }

  // a new instance of this class is created for every Run (access the current instance via `currentRun.reporting`)
  protected def PerRunReporting = new PerRunReporting
  class PerRunReporting extends PerRunReportingBase {
    /** Collects for certain classes of warnings during this run. */
    private class ConditionalWarning(what: String, doReport: Boolean, setting: Settings#Setting) {
      def this(what: String, booleanSetting: Settings#BooleanSetting) {
        this(what, booleanSetting.value, booleanSetting)
      }
      val warnings = mutable.LinkedHashMap[Position, (String, String)]()
      def warn(pos: Position, msg: String, since: String = "") =
        if (doReport) reporter.warning(pos, msg)
        else if (!(warnings contains pos)) warnings += ((pos, (msg, since)))
      def summarize() =
        if (warnings.nonEmpty && (setting.isDefault || doReport)) {
          val sinceAndAmount = mutable.TreeMap[String, Int]()
          warnings.valuesIterator.foreach { case (_, since) =>
            val value = sinceAndAmount.get(since)
            if (value.isDefined) sinceAndAmount += ((since, value.get + 1))
            else sinceAndAmount += ((since, 1))
          }
          val deprecationSummary = sinceAndAmount.size > 1
          sinceAndAmount.foreach { case (since, numWarnings) =>
            val warningsSince = if (since.nonEmpty) s" (since $since)" else ""
            val warningVerb   = if (numWarnings == 1) "was" else "were"
            val warningCount  = countElementsAsString(numWarnings, s"$what warning")
            val rerun         = if (deprecationSummary) "" else reporter.rerunWithDetails(setting, setting.name)
            reporter.warning(NoPosition, s"there ${warningVerb} ${warningCount}${warningsSince}${rerun}")
          }
          if (deprecationSummary) {
            val numWarnings   = warnings.size
            val warningVerb   = if (numWarnings == 1) "was" else "were"
            val warningCount  = countElementsAsString(numWarnings, s"$what warning")
            val rerun         = reporter.rerunWithDetails(setting, setting.name)
            reporter.warning(NoPosition, s"there ${warningVerb} ${warningCount} in total${rerun}")
          }
        }
    }

    // This change broke sbt; I gave it the thrilling name of uncheckedWarnings0 so
    // as to recover uncheckedWarnings for its ever-fragile compiler interface.
    private val _deprecationWarnings    = new ConditionalWarning("deprecation", settings.deprecation)
    private val _uncheckedWarnings      = new ConditionalWarning("unchecked", settings.unchecked)
    private val _featureWarnings        = new ConditionalWarning("feature", settings.feature)
    private val _inlinerWarnings        = new ConditionalWarning("inliner", !settings.optWarningsSummaryOnly, settings.optWarnings)
    private val _allConditionalWarnings = List(_deprecationWarnings, _uncheckedWarnings, _featureWarnings, _inlinerWarnings)

    // TODO: remove in favor of the overload that takes a Symbol, give that argument a default (NoSymbol)
    def deprecationWarning(pos: Position, msg: String, since: String): Unit = deprecationWarning(pos, NoSymbol, msg, since)
    def uncheckedWarning(pos: Position, msg: String): Unit   = _uncheckedWarnings.warn(pos, msg)
    def featureWarning(pos: Position, msg: String): Unit     = _featureWarnings.warn(pos, msg)
    def inlinerWarning(pos: Position, msg: String): Unit     = _inlinerWarnings.warn(pos, msg)

    def deprecationWarnings = _deprecationWarnings.warnings.toList
    def uncheckedWarnings   = _uncheckedWarnings.warnings.toList
    def featureWarnings     = _featureWarnings.warnings.toList
    def inlinerWarnings     = _inlinerWarnings.warnings.toList

    def allConditionalWarnings = _allConditionalWarnings flatMap (_.warnings)

    def deprecationWarning(pos: Position, sym: Symbol, msg: String, since: String): Unit = {
      println(s"Check $sym")
      val infractor = ""
      policy(pos, sym, msg, since, infractor) match {
        case policy.Silent =>
        case policy.Info   => reporter.echo(pos, s"msg (since $since)")
        case policy.Warn   => _deprecationWarnings.warn(pos, msg, since)
        case policy.Error  => reporter.error(pos, s"msg (since $since)")
      }
    }
    def deprecationWarning(pos: Position, sym: Symbol): Unit = {
      val version = sym.deprecationVersion.getOrElse("")
      val since   = if (version.isEmpty) version else s" (since $version)"
      val message = sym.deprecationMessage match { case Some(msg) => s": $msg"        case _ => "" }
      deprecationWarning(pos, sym, s"$sym${sym.locationString} is deprecated$since$message", version)
    }

    private[this] var reportedFeature = Set[Symbol]()
    def featureWarning(pos: Position, featureName: String, featureDesc: String, featureTrait: Symbol, construct: => String = "", required: Boolean): Unit = {
      val req     = if (required) "needs to" else "should"
      val fqname  = "scala.language." + featureName
      val explain = (
        if (reportedFeature contains featureTrait) "" else
        s"""|
            |----
            |This can be achieved by adding the import clause 'import $fqname'
            |or by setting the compiler option -language:$featureName.
            |See the Scaladoc for value $fqname for a discussion
            |why the feature $req be explicitly enabled.""".stripMargin
      )
      reportedFeature += featureTrait

      val msg = s"$featureDesc $req be enabled\nby making the implicit value $fqname visible.$explain" replace ("#", construct)
      // don't error on postfix in pre-0.13.18 xsbt/Compat.scala
      def isSbtCompat =
        (featureName == "postfixOps" && pos.source.path.endsWith("/xsbt/Compat.scala") && Thread.currentThread.getStackTrace.exists(_.getClassName.startsWith("sbt.")))
      if (required && !isSbtCompat) {
        reporter.error(pos, msg)
      } else featureWarning(pos, msg)
    }

    /** Has any macro expansion used a fallback during this run? */
    var seenMacroExpansionsFallingBack = false

    def summarizeErrors(): Unit = if (!reporter.hasErrors) {
      _allConditionalWarnings foreach (_.summarize())

      if (seenMacroExpansionsFallingBack)
        reporter.warning(NoPosition, "some macros could not be expanded and code fell back to overridden methods;"+
                "\nrecompiling with generated classfiles on the classpath might help.")

      // todo: migrationWarnings

      if (settings.fatalWarnings && reporter.hasWarnings)
        reporter.error(NoPosition, "No warnings can be incurred under -Xfatal-warnings.")
    }
  }
}
