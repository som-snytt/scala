
package scala.tools.nsc.util

import scala.util.{Failure, Success, Try}
import scala.util.chaining._

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

class StackTraceTest {
  final val debugging: Boolean = true
  def debug(e: Throwable): Unit = if (debugging) println(s"TRACE [\n${stackTraceString(e)}]")
  def debug(s: String): Unit    = if (debugging) println(s"PRETY [\n${s}\n]")

  val CausedBy   = "Caused by: "
  val Suppressed = "Suppressed: "

  // throws
  def sample = throw new RuntimeException("Point of failure")
  def sampler: String = sample

  // repackage with message
  def resample: String = try sample catch { case e: Throwable => throw new RuntimeException("resample", e) }
  def resampler: String = resample

  // simple wrapper
  def wrapper: String = try sample catch { case e: Throwable => throw new RuntimeException(e) }
  // another onion skin
  def rewrapper: String = try wrapper catch { case e: Throwable => throw new RuntimeException(e) }
  def rewrapperer: String = rewrapper

  // circular cause
  def insane: String = try sample catch {
    case e: Throwable =>
      val t = new RuntimeException(e)
      e initCause t
      throw t
  }
  def insaner: String = insane

  def repressed: String = try sample catch {
    case e: Throwable =>
      val t = new RuntimeException("My problem")
      t.addSuppressed(e)
      throw t
  }
  def represser: String = repressed

  // evaluating s should throw, p trims stack trace, test is invoked with resulting trace string
  def probe(s: => String)(p: StackTraceElement => Boolean)(test: String => Unit): Unit =
    Try(s).recover { case e => e.tap(debug).stackTracePrefixString(p) } match {
      case Success(s) => debug(s) ; test(s)
      case Failure(e) => throw e
    }

  //private def stackDepth = StackWalker.getInstance().walk(s => s.collect(Collectors.toList())).size()
  private def stackDepth = new Throwable().getStackTrace.length

  @Test def showsAllTrace(): Unit =
    probe(sampler)(_ => true)(s => assertTrue(s.linesIterator.length - stackDepth < 3))

  /* summary + one frame + elision
   *
  java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.sample(StackTraceTest.scala:19)
    at scala.tools.nsc.util.StackTraceTest.$anonfun$showsOnlyPrefix$1(StackTraceTest.scala:64)
    at scala.util.Try$.apply(Try.scala:210)
    at scala.tools.nsc.util.StackTraceTest.probe(StackTraceTest.scala:51)
    at scala.tools.nsc.util.StackTraceTest.showsOnlyPrefix(StackTraceTest.scala:64)
    [snip]
   *
  java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.sample(StackTraceTest.scala:19)
    ... 37 elided
   *
   */
  @Test def showsOnlyPrefix() =
    probe(sample)(_.getMethodName == "sample")(s => assertEquals(3, s.linesIterator.length))

  /* summary + one frame + elision, caused by + one frame + elision
   *
  java.lang.RuntimeException: resample
    at scala.tools.nsc.util.StackTraceTest.resample(StackTraceTest.scala:23)
    at scala.tools.nsc.util.StackTraceTest.resampler(StackTraceTest.scala:24)
    at scala.tools.nsc.util.StackTraceTest.$anonfun$showsCause$1(StackTraceTest.scala:75)
    at scala.util.Try$.apply(Try.scala:210)
    at scala.tools.nsc.util.StackTraceTest.probe(StackTraceTest.scala:51)
    at scala.tools.nsc.util.StackTraceTest.showsCause(StackTraceTest.scala:75)
    [snip]
    at java.base/java.lang.Thread.run(Thread.java:834)
  Caused by: java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.sample(StackTraceTest.scala:19)
    ... 39 more
   *
  java.lang.RuntimeException: resample
    at scala.tools.nsc.util.StackTraceTest.resample(StackTraceTest.scala:19)
    ... 38 elided
  Caused by: java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.sample(StackTraceTest.scala:15)
    ... 39 more
   */
  @Test def showsCause() = probe(resampler)(_.getMethodName != "resampler") { s =>
    val res = s.linesIterator.toList
    assertEquals(6, res.length)
    assertTrue(res.exists(_.startsWith(CausedBy)))
  }

  /* summary + one frame + elision times three
   *
  java.lang.RuntimeException: java.lang.RuntimeException: java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.rewrapper(StackTraceTest.scala:29)
    at scala.tools.nsc.util.StackTraceTest.rewrapperer(StackTraceTest.scala:30)
    at scala.tools.nsc.util.StackTraceTest.$anonfun$showsWrappedExceptions$1(StackTraceTest.scala:82)
    at scala.util.Try$.apply(Try.scala:210)
    at scala.tools.nsc.util.StackTraceTest.probe(StackTraceTest.scala:51)
    at scala.tools.nsc.util.StackTraceTest.showsWrappedExceptions(StackTraceTest.scala:82)
    [snip]
    at java.base/java.lang.Thread.run(Thread.java:834)
  Caused by: java.lang.RuntimeException: java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.wrapper(StackTraceTest.scala:27)
    ... 39 more
  Caused by: java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.sample(StackTraceTest.scala:19)
    ... 40 more
   *
  java.lang.RuntimeException: java.lang.RuntimeException: java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.rewrapper(StackTraceTest.scala:29)
    ... 38 elided
  Caused by: java.lang.RuntimeException: java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.wrapper(StackTraceTest.scala:27)
    ... 39 more
  Caused by: java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.sample(StackTraceTest.scala:19)
    ... 40 more
   */
  @Test def showsWrappedExceptions() = probe(rewrapperer)(_.getMethodName != "rewrapperer") { s =>
    val res = s.linesIterator.toList
    assertEquals(9, res.length)
    assertTrue(res.exists(_.startsWith(CausedBy)))
    assertEquals(2, res.collect { case s if s.startsWith(CausedBy) => s }.size)
  }

  /* summary + one frame + elision times two with extra frame
   *
  java.lang.RuntimeException: java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.insane(StackTraceTest.scala:35)
    at scala.tools.nsc.util.StackTraceTest.insaner(StackTraceTest.scala:39)
    at scala.tools.nsc.util.StackTraceTest.$anonfun$dontBlowOnCycle$1(StackTraceTest.scala:90)
    at scala.util.Try$.apply(Try.scala:210)
    at scala.tools.nsc.util.StackTraceTest.probe(StackTraceTest.scala:51)
    at scala.tools.nsc.util.StackTraceTest.dontBlowOnCycle(StackTraceTest.scala:90)
    [snip]
    at java.base/java.lang.Thread.run(Thread.java:834)
  Caused by: java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.sample(StackTraceTest.scala:19)
    at scala.tools.nsc.util.StackTraceTest.insane(StackTraceTest.scala:33)
    ... 38 more
    [CIRCULAR REFERENCE:java.lang.RuntimeException: java.lang.RuntimeException: Point of failure]
   *
  java.lang.RuntimeException: java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.insane(StackTraceTest.scala:35)
    ... 38 elided
  Caused by: java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.sample(StackTraceTest.scala:19)
    at scala.tools.nsc.util.StackTraceTest.insane(StackTraceTest.scala:33)
    ... 38 more
   *
   */
  @Test def dontBlowOnCycle() = probe(insaner)(_.getMethodName != "insaner") { s =>
    val res = s.linesIterator.toList
    assertEquals(7, res.length)
    assertTrue(res.exists(_.startsWith(CausedBy)))
  }

  /*
   *
  java.lang.RuntimeException: My problem
    at scala.tools.nsc.util.StackTraceTest.repressed(StackTraceTest.scala:43)
    at scala.tools.nsc.util.StackTraceTest.represser(StackTraceTest.scala:47)
    at scala.tools.nsc.util.StackTraceTest.$anonfun$showsSuppressed$1(StackTraceTest.scala:168)
    at scala.util.Try$.apply(Try.scala:210)
    at scala.tools.nsc.util.StackTraceTest.probe(StackTraceTest.scala:51)
    at scala.tools.nsc.util.StackTraceTest.showsSuppressed(StackTraceTest.scala:168)
    [snip]
    at java.base/java.lang.Thread.run(Thread.java:834)
    Suppressed: java.lang.RuntimeException: Point of failure
            at scala.tools.nsc.util.StackTraceTest.sample(StackTraceTest.scala:19)
            at scala.tools.nsc.util.StackTraceTest.repressed(StackTraceTest.scala:41)
            ... 38 more
   *
  java.lang.RuntimeException: My problem
    at scala.tools.nsc.util.StackTraceTest.repressed(StackTraceTest.scala:43)
    ... 38 elided
    Suppressed: java.lang.RuntimeException: Point of failure
      at scala.tools.nsc.util.StackTraceTest.sample(StackTraceTest.scala:19)
      at scala.tools.nsc.util.StackTraceTest.repressed(StackTraceTest.scala:41)
      ... 38 more
   */
  @Test def showsSuppressed() = probe(represser)(_.getMethodName != "represser") { s =>
    val res = s.linesIterator.toList
    assertEquals(7, res.length)
    assertTrue(res.exists(_.trim.startsWith(Suppressed)))
  }
}
