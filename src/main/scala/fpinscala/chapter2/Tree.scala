package fpinscala.chapter2


sealed trait Tree[+A] {

  def size: Int = {
    def loop(tree: Tree[A], total: Int): Int = tree match {
      case Nil                 => total
      case Leaf(_)             => total + 1
      case Branch(left, right) => 1 + loop(left,total) + loop(right,total)
    }
    loop(this, 0)
  }
}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

case object Nil extends Tree[Nothing]

object Tree{

  def head[A](that : Tree[A]) : A = that match {
    case Leaf(a) => a
    case Branch(l,_) => head(l)
  }

  def headLeaf[A](that : Tree[A]) : Tree[A] = that match {
    case Leaf(a) => Leaf(a)
    case Branch(l,_) => headLeaf(l)
  }

  def size2[A](that : Tree[A]): Int = that match {
      case Leaf(_)             => 1
      case Branch(left, right) => 1 + size2(left) + size2(right)
      case _                   => 0
  }

  def size[A,B](that : Tree[A]): Int = foldr(that)(0)( _ + size(_) + size(_))

  def foldr[A,B](tree : Tree[A])(z : B)(f : (B, Tree[A], Tree[A]) => B): B = tree match{
    case Branch(left, right) => f(z,left,right)
    case _                   => z
  }

  def maximum (tree : Tree[Int]): Int = foldr(tree)(head(tree))((t,l,r)=> maximum(l) max maximum(r))

  def depth[A](t : Tree[A] ) : Int = foldr(t)(0)((t,l,r) => 1 + ( depth(l) max depth(r)))
  //
  def map[A,B](t:Tree[A])(f : (A)=>B) : Tree[B]=
//    fold(t)(0)((t, left, right) =>  Branch(map(left)(f), map(right)(f)))
    t match {
    case Leaf(b)             =>  Leaf(f(b))
    case Branch(left, right) =>  Branch(map(left)(f), map(right)(f))
    case _ => Nil
  }

  def fold[A,B](tree : Tree[A])(z:B)(f :( A ) => B) : Tree[B]= tree match{
    case Leaf(a)             =>  Leaf( f(a) )
    case Branch(left, right) =>  Branch(fold(left)(z)(f), fold(right)(z)(f))
    case _ => Nil
  }




  def apply[A](as: A*): Tree[A] =
   Nil
}

