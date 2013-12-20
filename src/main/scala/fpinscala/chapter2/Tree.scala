package fpinscala.chapter2

import scala.util.Try


sealed trait Tree[+A] {

  def size: Int = {
    def loop(tree: Tree[A], total: Int): Int = tree match {
      case Nil => total
      case Leaf(_) => total + 1
      case Branch(left, right) => 1 + loop(left, total) + loop(right, total)
    }
    loop(this, 0)
  }
}


case class Leaf[A](value: A) extends Tree[A]

case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

case object Nil extends Tree[Nothing]

object Tree {


  def head[A](that: Tree[A]): Option[A] ={
    def loop(it:Tree[A]) : A = it match {
        case Leaf(a) => a
        case Branch(l, _) => loop(l)
      }
    Try(loop(that)).toOption
  }

  def headLeaf[A](that: Tree[A]): Tree[A] = that match {
    case Leaf(a) => Leaf(a)
    case Branch(l, _) => headLeaf(l)
    case _ => Nil
  }

  def size[A](that: Tree[A]): Int = fold(that)(a => 1)((a, b) => 1 + a + b)

  def maximum(tree: Tree[Int]): Int = fold(tree)(a => a)(_ max _)

  def depth[A](t: Tree[A]): Int = fold(t)(a => 0)((a, b) => 1 + (a max b))

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((a, b) => Branch(a, b))


  def apply[A](as: A*): Tree[A] =
    Nil //?
}

