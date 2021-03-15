package tasks

import u03.Streams.Stream
import u03.Streams.Stream._

import scala.annotation.tailrec

object StreamTask {
  @tailrec
  def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match {
    case (stream, 0) => stream
    case (Empty(), _) => empty()
    case (Cons(_, ft), n) => drop(ft())(n-1)
  }

  def constant[A](a: A): Stream[A] = iterate(a)(x => x)

  val fib: Stream[Int] = {
    def _fib(prev: Int, curr: Int): Stream[Int] = cons(prev, _fib(curr, prev+curr))
    _fib(0, 1)
  }
}
