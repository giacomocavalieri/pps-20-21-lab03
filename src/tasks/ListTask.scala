package tasks

import u02.Optionals._
import u02.Optionals.Option._
import u02.SumTypes._
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

  def max(l: List[Int]): Option[Int] = l match {
    case Cons(head, tail) => Some(Math.max(head, getOrElse(max(tail), Int.MinValue)))
    case Nil() => None()
  }

  def peopleToCourses(l: List[Person]): List[String] = flatMap(l)({
    case Teacher(_, course) => List.of(course)
    case _ => Nil()
  })
}
