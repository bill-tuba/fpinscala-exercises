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
  }

}
