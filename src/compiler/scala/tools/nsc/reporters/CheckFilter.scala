/* NSC -- new Scala compiler
 * Copyright 2018 LAMP/EPFL
 * @author A. P. Marki
 */

package scala.tools.nsc
package reporters

import scala.collection.mutable
import scala.tools.nsc.Settings
import scala.reflect.internal.{Reporter => InternalReporter}
import scala.reflect.internal.util.Position, Position.formatMessage
import java.io.PrintWriter

/** Filtering against existing check file.
 *
 *  Use whitelist.check to identify whitelisted messages.
 *
 *  In the absence of whitelist.check, produces reported.check,
 *  which is an accumulation of reported messages, with the goal
 *  of reporting only new messages at each run.
 *
 *  The check file is normal `DisplayReporter` output, which does
 *  not contain precise positions but only line numbers, so that
 *  the filter must generate the message in order to compare it
 *  against the current whitelist. When a message is deemed to
 *  be whitelisted, it is registered with `PositionFilter` so
 *  that it will not be forwarded for display.
 *
 *  TODO: an edit that changes message positions should be
 *  identifiable by git diff. That is, the diff should enable
 *  whitelisting at the new line position.
 */
trait CheckFiltering extends PositionFiltering {

  // whitelisted messages
  private var samples: List[String] = {
    import java.nio._, charset.StandardCharsets.UTF_8, file._, Files.readAllLines, Paths.{get => getPath}
    import scala.collection.JavaConverters._
    readAllLines(getPath("whitelist.check"), UTF_8).asScala.toList //.map(_.stripLineEnd)
  }

  /** If the formatted message was emitted in previous output,
   *  as attested by either the whitelist or reported.check file,
   *  then filter the message.
   */
  override protected def filter(pos: Position, msg: String, severity: Severity) = {
    val text = formatMessage(pos, DisplayReporter.explanation(msg), shortenFile = true)
    val test = text.lines.next
    println(s"Check [[$test]] against [[${samples.mkString("/")}]]")
    val suppress = samples.contains(test) && { testAndLog(pos, msg, severity) ; true }
    if (suppress) println(s"Suppressing [[$test]] against [[${samples.mkString("/")}]]")
    !suppress && super.filter(pos, msg, severity)
  }
}

/** A reporter that filters by position and whitelist, and then forwards for display. */
class CheckReporter(settings: Settings)
  extends PositionFilter(settings, DisplayReporter(settings, Console.in, new PrintWriter(Console.err, true), new PrintWriter(Console.out, true)))
  with CheckFiltering
  with CountingReporter
  with SummaryReporter
  with LimitFilter {
  private def displayReporter = delegate.asInstanceOf[DisplayReporter]
  def shortname_=(flag: Boolean): Unit = displayReporter.shortname = flag
  def shortname: Boolean = displayReporter.shortname
  def maxerrs = settings.maxerrs.value
  def maxwarns = settings.maxwarns.value
  def close(): Unit = displayReporter.close()
}
