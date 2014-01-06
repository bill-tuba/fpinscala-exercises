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
        Empty.takeWhile(x => true) shouldBe Stream.empty
        Empty.takeWhile(x => false) shouldBe Stream.empty
      }
      "Yield empty with all element on perpetually false predicate" in {
        Stream(1).takeWhile(x => false) shouldBe Stream.empty
        Stream(1).takeWhile(_ == 0) shouldBe Stream.empty
      }

      "Yields Stream minus elements up-to where condition failed" in {
        Stream(1).takeWhile(_ < 3).toList shouldBe List(1)
        Stream(1, 2).takeWhile(_ < 3).toList shouldBe List(1, 2)
        Stream(1, 2, 3).takeWhile(_ < 3).toList shouldBe List(1, 2)
      }

      "Exhaust Stream when always true" in {
        Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).takeWhile(x => true).toList shouldBe (1 to 10).toList
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

  "ForAll applies itself to all elements and" should {

    "fail on an Empty Stream" in {
      Stream[Int]().exists(x => x == 1) shouldBe false
      Stream[Int]().exists(x => x > 1) shouldBe false
      Stream[Int]().exists(x => x < 1) shouldBe false
      Stream[Int]().exists(x => x != 1) shouldBe false
    }

    "fail where predicate fails on any element" in {
      Stream(1, 2, 3).forAll(_ == 0) shouldBe false
      Stream(1, 2, 3).forAll(_ > 4) shouldBe false
      Stream(1, 2, 3).forAll(_ < 1) shouldBe false
      Stream(1, 2, 3).forAll(_ == 1) shouldBe false
      Stream(1, 2, 3).forAll(_ < 3) shouldBe false
    }

    "succeed where predicate evaluates to true for-all elements" in {
      Stream(1, 2, 3).forAll(x => x > 0 && x < 4) shouldBe false
      Stream(1, 2, 3).forAll(x => (1 to 3).contains(x)) shouldBe false
    }
  }

  "Fold-R" should {
    "be able to implement take-while" in {
      Stream(1).takeWhileWithFoldr(x => false).toList shouldBe Nil
      Stream(1).takeWhileWithFoldr(_ == 0).toList shouldBe Nil
      Stream(1).takeWhileWithFoldr(_ < 3).toList shouldBe List(1)
      Stream(1, 2).takeWhileWithFoldr(_ < 3).toList shouldBe List(1, 2)
      Stream(1, 2, 3).takeWhileWithFoldr(_ < 3).toList shouldBe List(1, 2)
      Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).takeWhileWithFoldr(x => true).toList shouldBe (1 to 10).toList
    }
  }

  "uncons in terms of foldRight" should {
    "be like our old uncons" in {
      // ? ? ?
    }
  }

  "Using foldRight you" should {
    "be able to implement map" in {
      Stream.empty[Int].map(_ + 1).toList shouldBe Nil
      Stream(1, 2, 3).map(_ + 1).toList shouldBe List(2, 3, 4)
      Stream(1, 2, 3).map(_.toString).toList shouldBe List("1", "2", "3")
    }
    "be able to implement filter" in {
      Stream.empty[Int].filter(_ > 0).toList shouldBe Nil
      Stream(1).filter(_ > 0).toList shouldBe List(1)
      Stream(1, 2, 3).filter(_ > 0).toList shouldBe List(1, 2, 3)
      Stream(1, 2, 3).filter(_ < 3).toList shouldBe List(1, 2)
    }
    "be able to implement append" in {
      Stream.empty[Int].append(Stream.empty).toList shouldBe Nil
      Stream.empty[Int].append(Stream(1)).toList shouldBe List(1)
      Stream(1).append(Stream.empty).toList shouldBe List(1)
      Stream(1, 2).append(Stream(3)).toList shouldBe List(1, 2, 3)
      Stream(1, 2).append(Stream(3, 4)).toList shouldBe List(1, 2, 3, 4)

    }
    "be able to implement flatMap" in {

    }
  }
}
