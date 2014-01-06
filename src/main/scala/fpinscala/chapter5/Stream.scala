package fpinscala.chapter5

sealed abstract class Stream[+A] {

  def uncons: Option[Cons[A]]

  def isEmpty: Boolean = uncons.isEmpty

  def take(i: Int): Stream[A] = uncons match {
    case None => Empty
    case Some(e) if i > 0 => e.tail.take(i - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = uncons match {
    case Some(e) if p(e.head) => e.tail.takeWhile(p)
    case _ => this
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B = uncons match {
    case Some(e) => f(e.head, e.tail.foldRight(z)(f))
    case None => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def toList: List[A] = {
    @scala.annotation.tailrec
    def loop(stream: Stream[A], list: List[A]): List[A] = stream.uncons match {
      case Some(cons) => loop(cons tail, list :+ cons.head)
      case _ => list
    }
    loop(this, Nil)
  }

  def toList2: List[A] = uncons match {
    case Some(c) => c.head :: c.tail.toList2
    case _ => Nil
  }
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

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  def empty[A]: Stream[A] = Empty

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = new Cons[A] {
    lazy val head = h
    lazy val tail = t
  }

}