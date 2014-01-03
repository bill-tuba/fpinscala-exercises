package fpinscala.chapter3

object OptionUtilityFunctions {

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(aa), Some(bb)) => Some(f(aa, bb))
      case _ => None
    }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  /**
   * Top secret formula for computing an annual car
   * insurance premium from two key factors.
   */
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age + 1.0 / numberOfSpeedingTickets

  def parseInsuranceRateQuote(age: String,
                              numberOfSpeedingTickets: String): Option[Double] = {

    val iAge = Try(age.toInt)
    val tickets = Try(numberOfSpeedingTickets.toInt)
    map2(iAge, tickets)(insuranceRateQuote)
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    //this is probably a fold
    def loop(list: List[Option[A]], ret: Some[List[A]]): Option[List[A]] = list match {
      case Nil => ret
      case Some(x) :: rest => loop(rest, Some(ret.get :+ x))
      case None :: _ => None
    }
    loop(a, Some(Nil))
  }

  //seq 2 and 3 were the 'correct' answers in the text - zoiks!!!1! boy was I off :)
  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence2(t) map (hh :: _))
    }

  def sequence3[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil)) {
      (opt, optList) => OptionUtilityFunctions.map2(opt, optList) {
        _ :: _
      }
    }
  }

  // 'my' traverse isn't the answer in the answers collection - yet it seems ok
  def traverse[A, B](input: List[A])(f: A => Option[B]): Option[List[B]] =
    input.foldRight(Some(Nil): Option[List[B]]) {
      (a, optionalList) =>
        f(a) flatMap (h => optionalList map (h :: _))
    }

  //  def sequence4[A](a: List[Option[A]]): Option[List[A]] = {
  //    traverse(a){;lp}
  //  }

}