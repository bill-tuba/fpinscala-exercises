package fpinscala.datastructures

import scala.util.Try


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

  def headOption: Option[A] = Try(this.head).toOption

  def tail = if (this.isEmpty) throw new Exception else drop(1)

  def tailOption: Option[List[A]] = Try(this.tail).toOption

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

  def reverse = foldLeft( Nil : List[A])( Cons(_ , _) )

  def take(n: Int): List[A] = {
    @scala.annotation.tailrec
    def loop(m: Int, taken: List[A], accum: List[A]): List[A] =
    if (m == 0 || taken.isEmpty) accum
    else loop(m - 1, taken.tail, Cons(taken.head, accum))
    loop(n, this, Nil).reverse
  }

  def length: Int = foldRight(0)((a, b) => b + 1)

  def init: List[A] = take(length - 1)

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

  def foldRight[B](z: B)(f: (A, B) => B) = {
    def loop(list: List[A], terminus: B)(fn: (A, B) => B): B = list match {
      case Nil => terminus
      case Cons(x, xs) => f(x, loop(xs, z)(f))
    }
    loop(this, z)(f)
  }

  def append[B >: A](implicit a2: List[B]): List[B] = {
    this match {
      case Nil => a2
      case Cons(h, t) => Cons(h, t.append(a2))
    }
  }

  def appendInTermsOfFoldRight[B >: A](implicit a2: List[B]): List[B] = {
    this.foldRight(a2)(Cons(_, _))
  }

  def appendInTermsOfFoldLeft[B >: A](implicit a2: List[B]): List[B] = {
    this.reverse.foldLeft(a2)(Cons(_, _))
  }

  def map[B >: A](f: A => B) : List[B] = {
    def loop(fromList: List[A], toList : List[B]) : List[B] = fromList match{
      case Nil => toList
      case Cons(h,t) => loop(fromList.tail, toList.append(List(f(h))))
    }
    loop(this, Nil)
  }

  def filter[B >: A](fn: A => Boolean) : List[B] = foldRight(Nil : List[B])((h,t) => if(fn(h)) Cons(h,t) else t)

  def filter2[B >: A](fn: A => Boolean) : List[B] =
    List.flatMap(this)((a) => if(fn(a)) List(a) else Nil)

  def mapAcross[B >: A](that : List[B])(z : B)( f :(B,B) => B) : List[B] = this match {
    case Cons(h,t) => Cons(List(h, that.head).foldLeft(z)(f), tail.mapAcross(that.tail)(z)(f))
    case Nil => Nil

  }

  def hasSubsequence[B >: A](that: List[B]): Boolean = {
    def loop(list1: List[A], list2: List[B], hasSub: Boolean): Boolean = (list1, list2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, if (h1 == h2) t2 else Cons(h2, t2), hasSub)
      case (_, Nil) => hasSub
      case (Nil, _) => false
    }
    loop(this, that, hasSub = true)
  }

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

  def product(list: List[Double]): Double = list.foldLeft(1.0)(_ * _)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def flatten[A](lists : List[List[A]]) : List[A] = {
    lists.foldRight(Nil : List[A])((tot,e) => tot.append(e) )
  }

  def concat[A](l: List[List[A]]): List[A] =
    l.foldRight(Nil:List[A])((tot,e) => tot.append(e) )


  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def map[A,B](l: List[A])(f: A => B): List[B] =
    l.foldRight(Nil:List[B])((h,t) => Cons(f(h),t))
  //    l.map(f)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))
}
