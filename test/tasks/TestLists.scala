package tasks

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import tasks.ListTask._
import u03.Lists._
import u03.Lists.List.append

class TestLists {
  @Test def testDrop(): Unit = {
    val list = List.of(10, 20, 30)
    assertEquals(List.of(20, 30), drop(list, 1))
    assertEquals(List.of(30), drop(list, 2))
    assertEquals(List.Nil(), drop(list, 3))
    assertEquals(List.Nil(), drop(list, 4))
  }

  @Test def testFlatMap(): Unit = {
    val list = List.of(10, 20)
    assertEquals(List.of(20, 40), flatMap(list)((x: Int) => List.of(2 * x)))
    assertEquals(append(List.of(10, 10), List.of(20, 20)), flatMap(list)(x => List.of(x, x)))
  }

  @Test def testMap(): Unit = {
    val list = List.of(10, 20)
    assertEquals(List.of(20, 40), map(list)(_ * 2))
    assertEquals(List.of(true, false), map(list)(_ < 15))
  }
}
