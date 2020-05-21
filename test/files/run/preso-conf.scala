
import scala.tools.nsc._, reporters._
import scala.reflect.internal.util.BatchSourceFile

object Test extends App {
  val settings = new Settings(_ => ())
  val reporter = new StoreReporter(settings)
  val pc = new interactive.Global(settings, reporter)
  val code = "class C { def f = 'hello }"
  pc.compileSources(List(new BatchSourceFile("code.scala", code)))
  assert(!reporter.hasWarnings)
  pc.runReporting.summarizeErrors()
  assert(reporter.hasWarnings)
}
