package fpinscala.datastructures

sealed trait List[+A] {

  override def toString: String = {
    def loop(list: List[Any]): String = list match {
      case Nil => ""
      case Cons(head, Nil) => head.toString
      case Cons(head, tail) => "%s,%s".format(head.toString, loop(tail))
    }
    "(%s)".format(loop(this))
  }

  def isEmpty: Boolean = this == Nil

  def head: A = this match {
    case Cons(head, _) => head
    case Nil => throw new Exception()
  }

  def headOption: Option[A] = Try(this.head)

  def tail = if (this.isEmpty) throw new Exception else drop(1)

  def tailOption: Option[List[A]] = Try(this.tail)

  def drop(n: Int): List[A] = (n, this) match {
    case (0, xs) => xs
    case (_, Nil) => Nil
    case (toDrop, Cons(_, xs)) => xs.drop(toDrop - 1)
  }

  def dropWhile(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case it if f(head) => it.drop(1).dropWhile(f)
    case rest => rest
  }

  def reverse = foldLeft( Nil : List[A])(Cons(_ , _))

  def take(n: Int): List[A] = {
    @scala.annotation.tailrec
    def loop(m: Int, taken: List[A], accum: List[A]): List[A] =
      if (m == 0 || taken.isEmpty) accum
      else loop(m - 1, taken.tail, Cons(taken.head, accum))
    loop(n, this, Nil).reverse
  }

  def length: Int = foldRight(0)((a, b) => b + 1)

  def init: List[A] = take(length - 1)

  private def Try[A](f: => A): Option[A] = try Some(f) catch {
    case e: Exception => None
  }


  /*classic fold left tail recursive*/
  def foldLeft[B](z: B)(fn: (A, B) => B) = {
    @scala.annotation.tailrec
    def loop(list: List[A], z: B)(fn: (A, B) => B): B = list match {
      case Nil => z
      case Cons(head, tail) => loop(tail, fn(head, z))(fn)
    }
    loop(this, z)(fn)
  }

  def foldLeftInTermsOfFoldRight[B](z: B)(f: (A, B) => B) = foldRight(z)(f)

  def foldRightInTermsOfFoldLeft[B](z: B)(f: (A, B) => B) = foldLeft(z)(f)
  
  /*classic fold right*/
  def foldRight[B](z: B)(f: (A, B) => B) = {
    def loop(list: List[A], terminus: B)(fn: (A, B) => B): B = list match {
      case Nil => terminus
      case Cons(x, xs
      ) => f(x, loop(xs, z)(f))
    }
    loop(this, z)(f)
  }


  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  //    def append(a2: List[A]): List[A] = {
  //     def loop(a1 : List[A], a2 : List[A]) : List[A]= a1 match {
  //        case Nil => a2
  //        case Cons(h,t) => Cons(h, t.append(a2))
  //      }
  //      loop(this, Nil)
  //    }

  //  def appendWithFoldRight(a2: List[A]): List[A] = {
//    def loop(a1 : List[A], a2 : List[A]) : List[A]= a1 match {
//      case Nil => a2
//      case Cons(h,t) => Cons(h, t.append(a2))
//    }
//    loop(this, Nil)
//  }
}

case object Nil extends List[Nothing]

case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {

  def fill[A](howMany: Int, what: A): List[A] = {
    @scala.annotation.tailrec
    def loop(loopsLeft: Int, list: List[A]): List[A] =
      if (loopsLeft == 0) list
      else loop(loopsLeft - 1, Cons(what, list))
    loop(howMany, List())
  }

  def setHead[A](newHead: A, list: List[A]): List[A] = Cons(newHead, list.tail)

  def sum(list: List[Int]): Int = list.foldLeft(0)(_ + _)

  def product(list: List[Double]): Double = list.foldLeft(1.0)(_ + _)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def appendInTermsOfFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    a1.foldRight(a2)( (a  ,b ) => Cons(a,b) )
  }

  def appendInTermsOfFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    a1.foldLeft(a2)( (a  ,b ) => Cons(a,b) )
  }
}
