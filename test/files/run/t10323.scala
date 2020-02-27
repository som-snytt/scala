import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
:paste < DONE
import X._
object X {
  def x = 42
}
DONE
x
  """.trim
}
