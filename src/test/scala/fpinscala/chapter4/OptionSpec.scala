package fpinscala.chapter4

import org.scalatest.{WordSpec, Matchers}

class OptionSpec extends WordSpec with Matchers {

  "Options" when {
    "map" should {
      "apply function on Some" in {
        Some(1).map(_ + 1) shouldEqual Some(2)
        Some(2.0).map(_ * 2) shouldEqual Some(4.0)
        Some("1").map(_ + "x") shouldEqual Some("1x")
      }
      "return None on None" in {
        (None: Option[Int]).map(_ * 2) shouldEqual None
      }
    }

    "flat-map" should {
      "given SOme apply function and return Some" in {
        Some(1).flatMap(a => Some(a + 1)) shouldEqual Some(2)
      }

      "given None not apply function and return None" in {
        (None: Option[Int]).flatMap(a => Some(a)) shouldEqual None
      }

    }
    "filtering" should {
      "yield None if None" in {
        (None: Option[Int]).filter(_ == 2) shouldEqual None
      }

      "yield None if predicate resolves to False" in {
        Some(1).filter(_ == 2) shouldEqual None
      }

      "yield Some if predicate resolves to True" in {
        Some(2).filter(_ == 2) shouldEqual Some(2)
      }
    }


    "Get Or Else" should {
      "return get-value on Some" in {
        Some(1).getOrElse(2) shouldEqual 1
      }
      "return else-value on None" in {
        None.getOrElse(2) shouldEqual 2
      }
    }
  }

  "Or Else evaluation" should {
    "take place for None" in {
      None.orElse(Some(2)) shouldEqual Some(2)
    }
    "not take place for Some" in {
      Some(1).orElse(Some(2)) shouldEqual Some(1)
    }
  }


}
