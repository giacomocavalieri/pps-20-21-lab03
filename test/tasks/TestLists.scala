package tasks

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import tasks.ListTask._
import u02.SumTypes._
import u03.Lists._
import u03.Lists.List.append
import u02.Optionals._

class TestLists {
  val testList: List[Int] = List.of(10, 20)

  @Test def testDrop(): Unit = {
    assertEquals(List.of(20), drop(testList, 1))
    assertEquals(List.Nil(), drop(testList, 2))
    assertEquals(List.Nil(), drop(testList, 3))
  }

  @Test def testFlatMap(): Unit = {
    assertEquals(List.of(20, 40), flatMap(testList)((x: Int) => List.of(2 * x)))
    assertEquals(append(List.of(10, 10), List.of(20, 20)), flatMap(testList)(x => List.of(x, x)))
  }

  @Test def testMap(): Unit = {
    assertEquals(List.of(20, 40), map(testList)(_ * 2))
    assertEquals(List.of(true, false), map(testList)(_ < 15))
  }

  @Test def testFilter(): Unit = {
    assertEquals(List.Nil(), filter(testList)(_ => false))
    assertEquals(testList, filter(testList)(_ => true))
    assertEquals(List.of(10), filter(testList)(_ < 15))
    assertEquals(List.of(20), filter(testList)(_ > 15))
  }

  @Test def testMax(): Unit = {
    assertEquals(Option.Some(20), max(testList))
    assertEquals(Option.None(), max(List.Nil()))
  }

  @Test def testPeopleToCourses(): Unit = {
    val people: List[Person] = List.of(Student("Student", 1998), Teacher("Teacher", "Course"))
    assertEquals(List.of("Course"), peopleToCourses(people))
    assertEquals(List.Nil(), peopleToCourses(List.Nil()))
  }

  @Test def testFoldLeft(): Unit = {
    assertEquals(0, foldLeft(List.Nil[Int]())(0)(_ + _))
    assertEquals(30, foldLeft(testList)(0)(_ + _))
    assertEquals(-30, foldLeft(testList)(0)(_ - _))
    assertEquals("1020", foldLeft(testList)("")((acc, elem) => acc+elem))
  }

  @Test def testFoldRight(): Unit = {
    assertEquals(0, foldRight(List.Nil[Int]())(0)(_ + _))
    assertEquals(30, foldRight(testList)(0)(_ + _))
    assertEquals(-10, foldRight(testList)(0)(_ - _))
    assertEquals("2010", foldRight(testList)("")((elem, acc) => acc+elem))
  }
}
