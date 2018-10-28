package scala.tools.nsc

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class MainRunnerTest {
  @Test
  def `command reports no class`: Unit = {
    import GenericRunnerCommand._
    var message: String = ""
    assertEquals(Error, new GenericRunnerCommand(List("Junk"), err => message = err).howToRun)
    assertEquals("No such file or class on classpath: Junk", message)
  }
  @Test
  def `scala Junk should fail`: Unit = {
    object TestGenericRunner extends MainGenericRunner {
      override def errorFn(str: String, e: Option[Throwable], isFailure: Boolean): Boolean = {
        assertTrue(isFailure)
        !isFailure
      }
    }

    assertFalse(TestGenericRunner.process(Array("Junk")))
  }
  @Test
  def `scala junk.jar should fail`: Unit = {
    assertFalse(new MainGenericRunner().process(Array("junk.jar")))
  }
}
