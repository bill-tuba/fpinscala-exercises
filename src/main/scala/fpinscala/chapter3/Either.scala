package fpinscala.chapter3

/**
 * Created with IntelliJ IDEA.
 * User: billa
 * Date: 1/3/14
 */
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(aa => b map (bb => f(aa, bb)))

  // or
  //for { aa <- this ;  bb <- b } yield f( aa, bb )

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = a match {
    case Nil => Right(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (tt => hh :: tt))
  }

  def sequence2[E, A](a: List[Either[E, A]]): Either[E, List[A]] = a match {
    case Nil => Right(Nil)
    case h :: t => for {hh <- h; tt <- sequence(t)} yield {
      hh :: tt
    }
  }

  def traverse[E, A, B](input: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    input.foldRight(Right(Nil): Either[E, List[B]]) {
      (x, xs) => f(x).flatMap {
        h => xs.map {
          t => h :: t
        }
      }
    }

  def traverse2[E, A, B](input: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    input.foldRight(Right(Nil): Either[E, List[B]]) {
      (x, xs) => for {h <- f(x); t <- xs} yield {
        h :: t
      }
    }

  def Try[E, A](a: => A): Either[E, A] =
    try Right(a)
    catch {
      case e: E => Left(e)
    }
}