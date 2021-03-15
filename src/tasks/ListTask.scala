package tasks

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
}
