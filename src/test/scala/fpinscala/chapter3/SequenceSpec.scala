package fpinscala.chapter3

import org.scalatest.Matchers
import org.scalatest.WordSpec
import OptionUtilityFns._

class SequenceSpec extends WordSpec with Matchers {

  "Exercise 4 ch 3" should {
    "List with all somes is an Option of Some(List)" in {
      sequence(List(Some(1))) shouldEqual Some(List(1))
      sequence(List(Some(1), Some(2))) shouldEqual Some(List(1, 2))
      sequence(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1, 2, 3))
    }

    "List sequence with any None is None" in {
      sequence(List(None)) shouldEqual None
      sequence(List(Some(1), None)) shouldEqual None
      sequence(List(Some(1), None, Some(2))) shouldEqual None
      sequence(List(Some(1), Some(2), Some(3), None)) shouldEqual None
      sequence(List(None, Some(1), Some(2), Some(3))) shouldEqual None
    }

  }


  "Traverse with any failure results in None" in {
    import fpinscala.chapter3.OptionUtilityFns.Try
    val sToInt = (s: String) => Try {
      s.toInt
    }

    traverse(List())(sToInt) shouldEqual Some(Nil)
    traverse(List("1"))(sToInt) shouldEqual Some(List(1))
    traverse(List("1", "2"))(sToInt) shouldEqual Some(List(1, 2))
    traverse(List("1", "y"))(sToInt) shouldEqual None
    traverse(List("x", "1"))(sToInt) shouldEqual None
    traverse(List("x", "y"))(sToInt) shouldEqual None
    traverse(List("1", "2", "3"))(sToInt) shouldEqual Some(List(1, 2, 3))

  }

}
