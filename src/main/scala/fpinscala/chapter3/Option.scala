package fpinscala.chapter3

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  def flatMap[B](f: (A => Option[B])): Option[B] = this match {
    case Some(a) => f(a)
    case _ => None
  }

  def flatMap2[B](f: (A => Option[B])): Option[B] = map(f) getOrElse None

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }

  def filter2(f: A => Boolean): Option[A] =  if (map(f) getOrElse false) this else None

  def filter3 (f: A => Boolean): Option[A] =  flatMap(a => if(f(a)) Some(a) else None)

  def orElse[B >: A](default: => Option[B]): Option[B] = this match {
    case None => default
    case _ => this
  }

  def orElse2[B >: A](default: => Option[B]): Option[B]  = if( this ==  None) default else this

  def orElse3[B >: A](default: => Option[B]): Option[B]  = filter( _ ==  None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]
