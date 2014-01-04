package fpinscala.chapter5

sealed abstract class Stream[+A] {
  def uncons: Option[Cons[A]]

  def isEmpty: Boolean = uncons.isEmpty
}

object Empty extends Stream[Nothing] {
  val uncons = None
}

sealed abstract class Cons[+A] extends Stream[A] {
  val head: A
  val tail: Stream[A]
  val uncons = Some(this)
}

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = new Cons[A] {
    lazy val head = h
    lazy val tail = t
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

}