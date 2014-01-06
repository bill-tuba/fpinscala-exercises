package fpinscala.chapter5

import org.scalatest.{Matchers, WordSpec}

class StreamSpec extends WordSpec with Matchers {


  "Streams" when {
    "constructed" can {
      "have no elements" in {
        Stream().isEmpty shouldBe true
        Stream().uncons shouldBe None
      }
      "Empty Streams are Empty" in {
        Stream() shouldBe Empty
      }
      "have some elements" in {
        Stream(1).isEmpty shouldBe false
        Stream(1).uncons shouldBe a[Some[Int]]
      }
    }
    "Unconsing of Streams" should {
      "Be None When Empty" in {
        Empty.uncons shouldBe None
      }
      "When Non Empty be Some" in {
        Stream(1).uncons shouldBe a[Some[Stream[Int]]]
      }
    }

    "To List on a Stream" should {
      "Yield Nil given Empty" in {
        Empty.toList shouldBe Nil
      }
      "Yield List of N elements for Stream" in {
        Stream().toList shouldBe Nil
        Stream(1).toList shouldBe List(1)
        Stream(1, 2).toList shouldBe List(1, 2)
        Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
      }
    }
    "Take on Stream" should {
      "Yields Nothing on Empty" in {
        Empty.take(0) shouldBe Empty
        Empty.take(1) shouldBe Empty
        Empty.take(-1) shouldBe Empty
      }
      "Yields Stream less Taken elements" in {
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

    "Take while predicate" should {
      "Yield Empty on Empty" in {
        Empty.takeWhile(x => true) shouldBe Empty
        Empty.takeWhile(x => false) shouldBe Empty
      }
      "Yield original Stream with all elemeent on perpetually false predicate" in {
        Stream(1).takeWhile(x => false).toList shouldBe List(1)
        Stream(1).takeWhile(_ == 0).toList shouldBe List(1)
      }

      "Yields Stream minus elements upto where condition failed" in {
        Stream(1).takeWhile(_ < 3) shouldBe Empty
        Stream(1, 2).takeWhile(_ < 3) shouldBe Empty
        Stream(1, 2, 3).takeWhile(_ < 3).toList shouldBe List(3)
      }

      "Exhaust Stream when always true" in {
        Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).takeWhile(x => true) shouldBe Empty
      }
    }
    "Folding" should {
      "yield a value" in {
        Stream[Int]().foldRight(0) {
          (a, b) => b + a
        } shouldBe 0
        Stream(1, 2, 3).foldRight(0) {
          (a, b) => b + a
        } shouldBe 6
        Stream(1, 2, 3).foldRight(false: Boolean) {
          (a, b) => a == 1 || b
        } shouldBe true
      }
    }
  }

  "Checking whether a condition exists" should {
    "always fail on an Empty Stream for predicate" in {
      Stream[Int]().exists(x => x == 1) shouldBe false
      Stream[Int]().exists(x => x > 1) shouldBe false
      Stream[Int]().exists(x => x < 1) shouldBe false
      Stream[Int]().exists(x => x != 1) shouldBe false
    }
    "evaluate to false if predicate fails" in {
      Stream(1, 2, 3).exists(_ == 0) shouldBe false
      Stream(1, 2, 3).exists(_ > 4) shouldBe false
      Stream(1, 2, 3).exists(_ < 1) shouldBe false
    }
    "evaluate to true if predicate if satisfied supplied any element" in {
      Stream(1, 2, 3).exists(_ == 1) shouldBe true
      Stream(1, 2, 3).exists(_ == 2) shouldBe true
      Stream(1, 2, 3).exists(_ == 3) shouldBe true
    }
  }
}
