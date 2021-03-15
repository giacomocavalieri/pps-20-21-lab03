package tasks

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.{BeforeEach, Test}
import u03.Lists._
import u03.Streams.Stream
import u03.Streams.Stream._

class TestStream {
  val testStream: Stream[Int] = take(iterate(1)(_ + 1))(3)

  @Test def testDrop(): Unit = {
    assertStreamEqualsList(List.of(2, 3), drop(1, testStream))
    assertStreamEqualsList(List.of(3), drop(2, testStream))
    assertStreamEqualsList(List.Nil(), drop(3, testStream))
    assertStreamEqualsList(List.Nil(), drop(4, testStream))
  }

  def assertStreamEqualsList[A](expected: List[A], actual: Stream[A]): Unit = {
    assertEquals(expected, toList(actual))
  }
}
