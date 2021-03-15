package tasks

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.{BeforeEach, Test}
import u03.Lists._
import u03.Streams.Stream
import u03.Streams.Stream._
import tasks.StreamTask._

class TestStream {
  val testStream: Stream[Int] = take(iterate(1)(_ + 1))(3)

  @Test def testDrop(): Unit = {
    assertStreamEqualsList(List.of(2, 3), drop(testStream)(1))
    assertStreamEqualsList(List.of(3), drop(testStream)(2))
    assertStreamEqualsList(List.Nil(), drop(testStream)(3))
    assertStreamEqualsList(List.Nil(), drop(testStream)(4))
  }

  @Test def testConstant(): Unit = {
    assertStreamEqualsList(List.of(1, 1, 1), take(constant(1))(3))
  }

  def assertStreamEqualsList[A](expected: List[A], actual: Stream[A]): Unit = {
    assertEquals(expected, toList(actual))
  }
}
