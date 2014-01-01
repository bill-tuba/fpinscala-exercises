package fpinscala.chapter3

import org.scalatest.Matchers
import org.scalatest.WordSpec

class LiftSpec extends WordSpec with Matchers {


  "Lifting functions" when {
    "wrapping an api" should {
      "result None" in {
        parseInsuranceRateQuote("1", "x") shouldEqual None
        parseInsuranceRateQuote("1", "1.") shouldEqual None
        parseInsuranceRateQuote("", "1") shouldEqual None
      }
      "result in the rate quote" in {
        parseInsuranceRateQuote("1", "1") shouldEqual Some(2.0)
      }
    }

  }

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
}
