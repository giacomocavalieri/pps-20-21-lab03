package tasks

object Lists {
  // A generic linkedlist
  sealed trait List[E]

  // a companion object (i.e., module) for List
  object List {
    private case class Cons[E](head: E, tail: List[E]) extends List[E]
    private case class Nil[E]() extends List[E]

    def cons[A](head: A, tail: List[A]): List[A] = Cons(head, tail)

    def empty[A](): List[A] = Nil()

    def of[A](a: A): List[A] = cons(a, empty())
    def of[A](a1: A, a2: A): List[A] = cons(a1, of(a2))
    def of[A](a1: A, a2: A, a3: A): List[A] = cons(a1, of(a2, a3))

    def sum(l: List[Int]): Int = l match {
      case Cons(h, t) => h + sum(t)
      case _ => 0
    }

    def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
      case (list, 0) => list
      case (Nil(), _) => empty()
      case (Cons(_, tail), n) => drop(tail, n-1)
    }

    def append[A](l1: List[A], l2: List[A]): List[A] = (l1, l2) match {
      case (Cons(h, t), l2) => Cons(h, append(t, l2))
      case _ => l2
    }

    def map[A,B](l: List[A])(mapper: A=>B): List[B] = l match {
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()
    }

    def filter[A](l1: List[A])(pred: A=>Boolean): List[A] = l1 match {
      case Cons(h,t) if (pred(h)) => Cons(h, filter(t)(pred))
      case Cons(_,t) => filter(t)(pred)
      case Nil() => Nil()
    }
  }
}
