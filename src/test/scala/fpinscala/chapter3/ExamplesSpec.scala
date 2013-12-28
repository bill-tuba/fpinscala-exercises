package fpinscala.chapter3

import org.scalatest.{Matchers, WordSpec}

import scala.{Option => _, Some => _, None => _}

class ExamplesSpec extends WordSpec with Matchers {

  "Finding a mean " when {
    "we have no numbers" should {
      "be None" in {
        mean(List()) shouldEqual None
      }
      "we have some numbers" should {
        "be Some mean" in {
          mean(List(1, 2, 3)) shouldEqual Some(2)
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


  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}



