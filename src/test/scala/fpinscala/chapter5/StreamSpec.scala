package fpinscala.chapter5

import org.scalatest.{Matchers, WordSpec}

class StreamSpec extends WordSpec with Matchers {


  "Streams" when {
    "constructed" can {
      "have no elements" in {
        Stream().isEmpty shouldBe true
        Stream().uncons shouldBe None
      }
      "have some elements" in {
        Stream(1).isEmpty shouldBe false
        Stream(1).uncons shouldBe a[Some[Int]]
      }
    }
    "expanded to a list" should {
      "be empty given no elements" in {
        Stream().toList shouldBe Nil
      }
      "be a list of n elements" in {
        Stream(1).toList shouldBe List(1)
        Stream(1, 2).toList shouldBe List(1, 2)
        Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
      }
    }
    "take" should {
      "take Nothing on Empty for any N" in {
        Stream().take(0) shouldBe Empty
        Stream().take(1) shouldBe Empty
      }
      "take N elements off Stream for N" in {
        Stream(1).take(0).toList shouldBe Stream(1).toList
        Stream(1).take(1) shouldBe Empty
        Stream(1, 2).take(1).toList shouldBe Stream(2).toList
        Stream(1, 2, 3).take(-1).toList shouldBe Stream(1, 2, 3).toList
        Stream(1, 2, 3).take(0).toList shouldBe Stream(1, 2, 3).toList
        Stream(1, 2, 3).take(1).toList shouldBe Stream(2, 3).toList
        Stream(1, 2, 3).take(2).toList shouldBe Stream(3).toList
        Stream(1, 2, 3).take(3) shouldBe Empty
        Stream(1, 2, 3).take(4) shouldBe Empty

      }

    }
  }

}
