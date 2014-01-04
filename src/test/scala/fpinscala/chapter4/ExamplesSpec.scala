package fpinscala.chapter4

import org.scalatest.{Matchers, WordSpec}

import scala.{Option => _, Some => _, None => _}

class ExamplesSpec extends WordSpec with Matchers {

  "A Sequence's mean" when {
    "empty" should {
      "be undefined" in {
        mean(List()) shouldEqual None
      }
      "non-empty" should {
        "be the mean" in {
          mean(List(1, 2, 3)) shouldEqual Some(2)
        }
      }
    }
  }

  "A Sequence Variance" when {
    "empty" should {
      "be undefined" in {
        variance(List()) shouldEqual None
      }
      "not empty" should {
        "be the mean of means" in {
          variance(List(1, 2, 3)) shouldEqual Some(2.0 / 3.0)
        }
      }
    }
  }

  "An Option" when {
    "we do a get" should {
      "be a value for Some" in {
        Some(1).get shouldEqual 1
      }
    }
  }


  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}



