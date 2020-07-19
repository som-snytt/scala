package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
class NumericRangeTest {

  @Test
  def emptyiterator: Unit = {
    assertFalse(NumericRange(1, 0, 1).iterator.hasNext)
    assertFalse(NumericRange(0, 10, -1).iterator.hasNext)
  }

  @Test
  def nonEmptyiterator: Unit = {
    val it = NumericRange(0, 3, 1).iterator

    assertTrue(it.hasNext)
    assertEquals(0, it.next())
    assertTrue(it.hasNext)
    assertEquals(1, it.next())
    assertTrue(it.hasNext)
    assertEquals(2, it.next())
    assertFalse(it.hasNext)
  }

  @Test
  def t11163_BigDecimalSum: Unit = {
    val x = (BigDecimal(1) to BigDecimal(3) by 1).sum
    assertEquals(BigDecimal(6), x)
  }

  @Test
  def t11152_BigDecimalMakesProgress: Unit = {
    // Overflow case with default MathContext
    val a = BigDecimal(1)
    val b = a + BigDecimal("1e-30")
    val c = BigDecimal("1e-38")
    assertThrows[IllegalArgumentException]((a to b by c).length < 0)    // Force evaluation of `length`

    // Same math, greater precision--should be no overflow
    val aa = BigDecimal(1, new java.math.MathContext(40))
    val bb = aa + BigDecimal("1e-30")
    assertEquals(
      Some(BigDecimal("1." + "0"*37 + "1")),
      (aa to bb by c).drop(1).headOption,
    )

    // Make sure that positive/negative switch works okay at the limit of precision
    val mc2 = new java.math.MathContext(2)
    val nr = (BigDecimal("-7", mc2) to BigDecimal("7", mc2) by BigDecimal("0.1", mc2))
    val upscale = -70 to 70 by 1
    assertEquals(nr.length, upscale.length)
    assertTrue(
      (nr zip upscale).forall{ case (x, x10) => x*10 == x10 }
    )

    // Make sure that we catch it right past the limit of precision
    assertThrows[IllegalArgumentException] {
      val mc2 = new java.math.MathContext(2)
      val nr = (BigDecimal("-7", mc2) to BigDecimal("7", mc2) by BigDecimal("0.01", mc2))
      val upscale = -700 to 700 by 1
      nr.zip(upscale).forall { case (x, x100) => x*100 == x100 }
    }
  }

  @Test
  def `t12073`(): Unit = {
    val start = BigDecimal(-2E-34)
    val inc = BigDecimal(1E-34)
    val end = BigDecimal(1E-64)
    val ok  = List.unfold(start)(prec => Option.when(prec < end)(prec -> (prec + inc)))
    println(ok)
    val bad = Range.BigDecimal(start, end, inc).toList
    println(bad)
  }
}
