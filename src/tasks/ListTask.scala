package tasks

import u02.Optionals._
import u03.Lists._
import u03.Lists.List._

import scala.annotation.tailrec

object ListTask {
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (list, 0) => list
    case (Nil(), _) => Nil()
    case (Cons(_, tail), n) => drop(tail, n-1)
  }

  def flatMap[A, B](l: List[A])(mapper: A => List[B]): List[B] = l match {
    case Cons(head, tail) => append(mapper(head), flatMap(tail)(mapper))
    case Nil() => Nil()
  }

  def map[A, B](l: List[A])(mapper: A => B): List[B] = flatMap(l)(a => List.of(mapper(a)))

  def filter[A](l: List[A])(predicate: A => Boolean): List[A] = flatMap(l)({
    case a if predicate(a) => List.of(a)
    case _ => Nil()
  })

  def max(l: List[Int]): Option[Int] = {
    @tailrec
    def findMax(list: List[Int], currentMax: Int): Int = (list, currentMax) match {
      case (Nil(), currentMax) => currentMax
      case (Cons(head, tail), currentMax) if head > currentMax => findMax(tail, head)
      case (Cons(_, tail), currentMax) => findMax(tail, currentMax)
    }

    l match {
      case Cons(head, tail) => Option.Some(findMax(tail, head))
      case Nil() => Option.None()
    }
  }
}
