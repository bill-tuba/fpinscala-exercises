package fpinscala.chapter3

sealed trait Option[+A] {

  def map[B >: A](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def flatMap[B](f : (A => Option[B])) : Option[B] = this match{
    case Some(a) => f(a)
    case None => None
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => Some(a)
    case _ => None
  }

  def getOrElse[B >: A](default : => B ) : B = this match {
    case Some(a) => a
    case _ => default
  }
  def orElse[B >: A] ( default : => Option[B]) : Option[B] = this match {
    case None => default
    case _ => this
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]
