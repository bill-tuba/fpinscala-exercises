package fpinscala.chapter4

import org.scalatest.Matchers
import org.scalatest.WordSpec
import OptionUtilityFunctions._

class OptionUtilityFunctionsSpec extends WordSpec with Matchers {

  "Sequencing" should {
    "result in Some(x) where input is all Somes" in {
      sequence(List(Some(1))) shouldEqual Some(List(1))
      sequence(List(Some(1), Some(2))) shouldEqual Some(List(1, 2))
      sequence(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1, 2, 3))
    }

    "result in None where any input is None" in {
      sequence(List(None)) shouldEqual None
      sequence(List(Some(1), None)) shouldEqual None
      sequence(List(Some(1), None, Some(2))) shouldEqual None
      sequence(List(Some(1), Some(2), Some(3), None)) shouldEqual None
      sequence(List(None, Some(1), Some(2), Some(3))) shouldEqual None
    }
  }

  "Traverse" should {
    "results in None when any input fails" in {
      val sToInt = (s: String) => Try[Int, Exception](s.toInt)
      traverse(List())(sToInt) shouldEqual Some(Nil)
      traverse(List("1"))(sToInt) shouldEqual Some(List(1))
      traverse(List("1", "2"))(sToInt) shouldEqual Some(List(1, 2))
      traverse(List("1", "y"))(sToInt) shouldEqual None
      traverse(List("x", "1"))(sToInt) shouldEqual None
      traverse(List("x", "y"))(sToInt) shouldEqual None
      traverse(List("1", "2", "3"))(sToInt) shouldEqual Some(List(1, 2, 3))
    }
  }
}
