package fpinscala.chapter5

sealed abstract class Stream[+A] {
  import Stream._

  def uncons: Option[Cons[A]]

  def isEmpty: Boolean = uncons.isEmpty

  def take(n: Int): Stream[A] =
    if (n > 0) uncons match {
      case Some(c) if n == 1  => cons(c.head, Stream())
      case Some(c)            => cons(c.head, c.tail.take(n-1))
      case _                  => empty
    }else empty


  def takeWhile(f: A => Boolean): Stream[A] = uncons match {
    case Some(c) if f(c.head) => cons(c.head, c.tail takeWhile f)
    case _ => empty
  }

  def takeWhileWithFoldr(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) {
      (h, t) => if (p(h)) cons(h, t) else empty
    }

  def foldRight[B](z: B)(f: (A, => B) => B): B = uncons match {
    case Some(e) => f(e.head, e.tail.foldRight(z)(f))
    case None => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) && b)

  def map[B](f: (A) => B): Stream[B] =
    foldRight(empty[B]) { (h, t) => cons(f(h), t) }

  def filter[B](f: A => Boolean): Stream[A] =
    foldRight(empty[A])(  (h,t) => if (f(h)) cons(h, t)   else t)

  def append[B >: A](implicit a2: Stream[B]): Stream[B] =
    foldRight(a2) {(h, t) => cons(h, t)}

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]){ (h,t) => f(h).append(t)}

  def find(p: A => Boolean): Option[A] = filter(p).uncons.map(_.head)

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
  val ones: Stream[Int] = cons(1, ones)

}