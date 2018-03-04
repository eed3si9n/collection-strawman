package strawman.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import strawman.collection.immutable.NumericRange

@RunWith(classOf[JUnit4])
class NumericRangeTest {

  @Test
  def emptyIterator(): Unit = {
    assertFalse(NumericRange(1, 0, 1).iterator().hasNext)
    assertFalse(NumericRange(0, 10, -1).iterator().hasNext)
  }

  @Test
  def nonEmptyIterator(): Unit = {
    val it = NumericRange(0, 3, 1).iterator()

    assertTrue(it.hasNext)
    assertEquals(0, it.next())
    assertTrue(it.hasNext)
    assertEquals(1, it.next())
    assertTrue(it.hasNext)
    assertEquals(2, it.next())
    assertFalse(it.hasNext)
  }

  @Test
  def doubleRangeToList(): Unit = {
    import scala.math.Numeric._
    val r = NumericRange.inclusive(1.0, 2.0, 0.2)(DoubleAsIfIntegral)
    val l = NumericRange.inclusive(1.0, 2.0, 0.2)(DoubleAsIfIntegral).toList

    assertEquals(s"$l", 6, l.size)
    assertEquals(1.0, l(0), 0.00001)
    assertEquals(1.2, l(1), 0.00001)
    assertEquals(1.4, l(2), 0.00001)
    assertEquals(1.6, l(3), 0.00001)
    assertEquals(1.8, l(4), 0.00001)
    assertEquals(2.0, l(5), 0.00001)
    // println(l)

    assertEquals(r(0), l(0), 0.00001)
    assertEquals(r(1), l(1), 0.00001)
    assertEquals(r(2), l(2), 0.00001)
    assertEquals(r(3), l(3), 0.00001)
    assertEquals(r(4), l(4), 0.00001)
    assertEquals(r(5), l(5), 0.00001)
  }

  @Test
  def doubleRangeDrop(): Unit = {
    import scala.math.Numeric._
    val r = NumericRange.inclusive(0.0, 1.0, 0.1)(DoubleAsIfIntegral)
    val l = NumericRange.inclusive(0.0, 1.0, 0.1)(DoubleAsIfIntegral).toList

    assertEquals(r.toList.take(3)(2), r.toList.take(3)(2), 0.00001)
    assertEquals(r.toList.drop(3)(7), r.toList.drop(3)(7), 0.00001)

    val x1 = r.toList.drop(3).take(3)
    val x2 = r.drop(3).take(3).toList
    assertEquals(x1(0), 0.3, 0.00001)
    assertEquals(x1(1), 0.4, 0.00001)
    assertEquals(x1(2), 0.5, 0.00001)
    assertEquals(x2(0), 0.3, 0.00001)
    assertEquals(x2(1), 0.4, 0.00001)
    assertEquals(x2(2), 0.5, 0.00001)
  }

  @Test
  def doubleRangeContains(): Unit = {
    val start = System.currentTimeMillis
    import scala.math.Numeric._
    val r = NumericRange.inclusive(0.0, 1000.0, 0.1)(DoubleAsIfIntegral)
    // println(r.toList)
    for { i <- 0 to 10000 } {
      assertEquals(r.indexOf(r(i)), i)
      assertEquals(r.contains(r(i)), true)
    }
    val estimatedTime = System.currentTimeMillis - start
    println(estimatedTime + " ms")
  }

  @Test
  def doubleRangeUneven(): Unit = {
    import scala.math.Numeric._
    val r = NumericRange(1.0, 5.0, 1.1)(DoubleAsIfIntegral)
    val l = r.toList
    assertEquals(s"$l", 4, l.size)
    assertEquals(1.0, l(0), 0.00001)
    assertEquals(2.1, l(1), 0.00001)
    assertEquals(3.2, l(2), 0.00001)
    assertEquals(4.3, l(3), 0.00001)
  }
}
