package tasks

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import tasks.Lists._
import tasks.Lists.List._

class TestLists {
  @Test def testDrop(): Unit = {
    val list = List.of(10, 20, 30)
    assertEquals(List.of(20, 30), drop(list, 1))
    assertEquals(List.of(30), drop(list, 2))
    assertEquals(List.empty(), drop(list, 3))
    assertEquals(List.empty(), drop(list, 4))
  }

  @Test def testFlatMap(): Unit = {
    val list = List.of(10, 20)
    assertEquals(List.of(20, 40), flatMap(list)((x: Int) => List.of(2 * x)))
  }
}
