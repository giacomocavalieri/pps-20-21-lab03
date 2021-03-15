package tasks

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import tasks.ListTask._
import u02.SumTypes._
import u03.Lists._
import u03.Lists.List.append
import u02.Optionals._

class TestLists {
  val list = List.of(10, 20)

  @Test def testDrop(): Unit = {
    assertEquals(List.of(20), drop(list, 1))
    assertEquals(List.Nil(), drop(list, 2))
    assertEquals(List.Nil(), drop(list, 3))
  }

  @Test def testFlatMap(): Unit = {
    assertEquals(List.of(20, 40), flatMap(list)((x: Int) => List.of(2 * x)))
    assertEquals(append(List.of(10, 10), List.of(20, 20)), flatMap(list)(x => List.of(x, x)))
  }

  @Test def testMap(): Unit = {
    assertEquals(List.of(20, 40), map(list)(_ * 2))
    assertEquals(List.of(true, false), map(list)(_ < 15))
  }

  @Test def testFilter(): Unit = {
    assertEquals(List.Nil(), filter(list)(_ => false))
    assertEquals(list, filter(list)(_ => true))
    assertEquals(List.of(10), filter(list)(_ < 15))
    assertEquals(List.of(20), filter(list)(_ > 15))
  }

  @Test def testMax(): Unit = {
    assertEquals(Option.Some(20), max(list))
    assertEquals(Option.None(), max(List.Nil()))
  }

  @Test def testPeopleToCourses(): Unit = {
    val people: List[Person] = List.of(Student("Student", 1998), Teacher("Teacher", "Course"))
    assertEquals(List.of("Course"), peopleToCourses(people))
    assertEquals(List.Nil(), peopleToCourses(List.Nil()))
  }

  @Test def testFoldLeft(): Unit = {
    assertEquals(0, foldLeft(List.Nil[Int]())(0)(_ + _))
    assertEquals(30, foldLeft(list)(0)(_ + _))
    assertEquals(-30, foldLeft(list)(0)(_ - _))
  }
}
