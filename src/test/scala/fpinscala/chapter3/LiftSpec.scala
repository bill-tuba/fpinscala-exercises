package fpinscala.chapter3

import org.scalatest.Matchers
import org.scalatest.WordSpec
import fpinscala.chapter3.LiftsSeqsTraversalAndTry._


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
}
