package fpinscala.chapter5

import org.scalatest.{Matchers, WordSpec}


import scala.Some

class StreamSpec extends WordSpec with Matchers {
  import Stream.{empty => anEmpty, _ }
  
  def stream0   = Stream[Int]()
  def stream1   = Stream(1)
  def stream2   = Stream(1,2)
  def stream3   = Stream(1,2,3)
  def stream10  = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  "Streams" when {
    "constructed" can {
      "have no elements" in {
        Stream().isEmpty  shouldBe true
        Stream().uncons   shouldBe None
      }
      "Empty Streams are Empty" in {
        Stream()          shouldBe Empty
      }
      "have some elements" in {
        stream1.isEmpty   shouldBe false
        stream1.uncons    shouldBe a[Some[Int]]
      }
    }
    "Unconsing of Streams" should {
      "Be None When Empty" in {
        Empty  .uncons    shouldBe None
        anEmpty.uncons    shouldBe None
      }

      "When Non Empty be Some" in {
        stream1.uncons    shouldBe a[Some[Stream[Int]]]
      }
    }

    "To List on a Stream" should {
      "Yield Nil given Empty" in {
        Empty           .toList shouldBe Nil
        anEmpty         .toList shouldBe Nil
      }
      "Yield List of N elements for Stream" in {
        Stream()        .toList shouldBe Nil
        stream1         .toList shouldBe List(1)
        stream2         .toList shouldBe List(1, 2)
        stream3         .toList shouldBe List(1, 2, 3)
      }
    }
    "Take on Stream" should {
      "Yields Nothing on Empty" in {
        Empty.take( 0)          shouldBe Empty
        Empty.take( 1)          shouldBe Empty
        Empty.take(-1)          shouldBe Empty
      }
      "Yields Stream less Taken elements" in {
        stream1.take(0) .toList shouldBe Nil
        stream1.take(1) .toList shouldBe List(1)
        stream2.take(1) .toList shouldBe stream1.toList
        stream3.take(-1).toList shouldBe Nil
        stream3.take(0) .toList shouldBe Nil
        stream3.take(1) .toList shouldBe stream1.toList
        stream3.take(2) .toList shouldBe stream2.toList
        stream3.take(3) .toList shouldBe List(1,2,3)
        stream3.take(4) .toList shouldBe List(1,2,3)
      }
    }

    "Take while predicate" should {
      "Yield Empty on Empty" in {
        Empty  .takeWhile(x => true )   shouldBe anEmpty
        Empty  .takeWhile(x => false)   shouldBe anEmpty
      }
      "Yield empty with all element on perpetually false predicate" in {
        stream1.takeWhile(x => false)   shouldBe anEmpty
        stream1.takeWhile(_ == 0    )   shouldBe anEmpty
      }

      "Yields Stream minus elements up-to where condition failed" in {
        stream1.takeWhile(_ < 3).toList shouldBe List(1)
        stream2.takeWhile(_ < 3).toList shouldBe List(1, 2)
        stream3.takeWhile(_ < 3).toList shouldBe List(1, 2)
      }

      "Exhaust Stream when always true" in {
        stream10.takeWhile(x => true).toList shouldBe (1 to 10).toList
      }
    }
    "Folding" should {
      "yield a value" in {
        anEmpty[Int]    .foldRight(0){_+_} shouldBe 0
        stream3.foldRight(0){ _+_} shouldBe 6
      }
    }
  }

  "Checking whether a condition exists" should {
    "always fail on an Empty Stream for predicate" in {
      stream0.exists(x => x == 1) shouldBe false
      stream0.exists(x => x >  1) shouldBe false
      stream0.exists(x => x <  1) shouldBe false
      stream0.exists(x => x != 1) shouldBe false
    }
    "evaluate to false if predicate fails" in {
      stream3.exists(_ == 0)      shouldBe false
      stream3.exists(_ >  4)      shouldBe false
      stream3.exists(_ <  1)      shouldBe false
    }
    "evaluate to true if predicate if satisfied supplied any element" in {
      stream3.exists(_ == 1)      shouldBe true
      stream3.exists(_ == 2)      shouldBe true
      stream3.exists(_ == 3)      shouldBe true
    }
  }

  "ForAll applies itself to all elements and" should {

    "fail on an Empty Stream" in {
      stream0.exists(x => x == 1) shouldBe false
      stream0.exists(x => x >  1) shouldBe false
      stream0.exists(x => x <  1) shouldBe false
      stream0.exists(x => x != 1) shouldBe false
    }

    "fail where predicate fails on any element" in {
      stream3.forAll(_ == 0)    shouldBe false
      stream3.forAll(_ >  4)    shouldBe false
      stream3.forAll(_ <  1)    shouldBe false
      stream3.forAll(_ == 1)    shouldBe false
      stream3.forAll(_ <  3)    shouldBe false
    }

    "succeed where predicate evaluates to true for-all elements" in {
      stream3.forAll(x => x > 0 && x < 4)       shouldBe true
      stream3.forAll(x => (1 to 3).contains(x)) shouldBe true
      stream3.forAll(x => (1 to 2).contains(x)) shouldBe false
      stream3.forAll(x => x < 0 && x > 4)       shouldBe false
    }
  }

  "Fold-R" should {
    "be able to implement take-while" in {
      stream1.takeWhileWithFoldr(x => false)  .toList shouldBe Nil
      stream1.takeWhileWithFoldr(_ == 0)      .toList shouldBe Nil
      stream1.takeWhileWithFoldr(_ < 3)       .toList shouldBe List(1)
      stream2.takeWhileWithFoldr(_ < 3)       .toList shouldBe List(1, 2)
      stream3.takeWhileWithFoldr(_ < 3)       .toList shouldBe List(1, 2)
      stream10.takeWhileWithFoldr(x => true)  .toList shouldBe (1 to 10).toList
    }
  }

  "uncons in terms of foldRight" should {
    "be like our old uncons" in {
      // ? ? ?
    }
  }

  "Using foldRight you" should {

    "be able to implement map" in {
      stream0 .map( _ + 1)     .toList shouldBe Nil
      stream3 .map( _ + 1)     .toList shouldBe List(2, 3, 4)
      stream3 .map( _.toString).toList shouldBe List("1", "2", "3")
    }

    "be able to implement filter" in {
      stream0.filter( _ > 0)   .toList shouldBe Nil
      stream1.filter( _ > 0)   .toList shouldBe List(1)
      stream3.filter( _ > 0)   .toList shouldBe List(1, 2, 3)
      stream3.filter( _ < 3)   .toList shouldBe List(1, 2)
    }

    "be able to implement append" in {
      stream0.append( anEmpty) .toList shouldBe Nil
      stream0.append( stream1) .toList shouldBe List(1)
      stream1.append( anEmpty) .toList shouldBe List(1)
      stream2.append( Stream(3)).toList shouldBe List(1, 2, 3)
      stream2.append( Stream(3, 4)).toList shouldBe List(1, 2, 3, 4)
    }

    "be able to implement flatMap" in {
      stream0.flatMap( i=>  Stream(i)       ).toList shouldBe Nil
      stream3.flatMap( i => Stream(i + 1)   ).toList shouldBe List(2, 3, 4)
      stream3.flatMap( i => Stream(i, i + 1)).toList shouldBe List(1,2,2,3,3,4)
    }
  }

  "With our HOFs in place we" should {
    "be able to compose a lazy chain" in{
      def stream: Stream[Int] =
        stream10.filter(_ < 5).map(_ + 1).filter(_ % 2 == 0)
      stream.take(2)      .toList shouldBe List(2,4)
      stream              .toList shouldBe List(2,4)
      stream.take(0)      .toList shouldBe Nil
      stream.take(1)      .toList shouldBe List(2)
      stream.foldRight(1){_ * _ } shouldBe 8
    }
    "reordering the composed stream gets different results" in{
      def aStream: Stream[Int] = stream10
        .filter(_ % 2 == 0).map(_ + 1).filter(_ < 5)

      aStream            .toList  shouldBe List(3)
      aStream.take(0)    .toList  shouldBe Nil
      aStream.take(1)    .toList  shouldBe List(3)
      aStream.foldRight(1){_ * _ }shouldBe 3

      def sameContentsWithDifferentOrderOfOps: Stream[Int] =
        stream10.filter(_ % 2 == 0).filter(_ < 5).map(_ + 1)
      sameContentsWithDifferentOrderOfOps             .toList  shouldBe List(3,5)
      sameContentsWithDifferentOrderOfOps.take(0)     .toList  shouldBe Nil
      sameContentsWithDifferentOrderOfOps.take(1)     .toList  shouldBe List(3)
      sameContentsWithDifferentOrderOfOps.foldRight(1){_ * _ } shouldBe 15
    }
  }
  "Find" should {
    "get the first element matching predicate p" in{
      stream0  .find( _ <  3) shouldBe None
      stream3  .find( _ <  3) shouldBe Some(1)
      stream1.find( _ <  1)   shouldBe None
      stream3  .find( _ == 2) shouldBe Some(2)
    }
  }
  "Infinite Streams" should {
    "be able to have hofs applied against it" in {
      infinity.take(5)    .exists( _ == 1   )    shouldBe true
      infinity.map(_ + 1) .exists(_ % 2 == 0)    shouldBe true
      infinity.takeWhile(_ == 1).take(1)         shouldBe a [Stream[Int]]
      infinity.forAll(_ != 1)                    shouldBe false
    }

    "generalize infinite to a fn" in {
      constant(1).take(5)    .exists( _ == 1   ) shouldBe true
      constant(1).map(_ + 1) .exists(_ % 2 == 0) shouldBe true
      constant(1).takeWhile(_ == 1).take(1)      shouldBe a [Stream[Int]]
      constant(1).forAll(_ != 1)                 shouldBe false
    }
    "infinite list makes for infinite fun" in{
      infinity.take(5)    .toList               shouldBe List.fill(5)(1)
      infinity.take(1000) .toList               shouldBe List.fill(1000)(1)
    }
    "create an incrementing series" in{
      from(0).take(1)     .toList               shouldBe List(0)
      from(1).take(1)     .toList               shouldBe List(1)
      from(1).take(2)     .toList               shouldBe List(1,2)
      from(1).take(3)     .toList               shouldBe List(1,2,3)
      from(2).take(2)     .toList               shouldBe List(2,3)
    }

    "fibs" in {
      fibs.take(1)        .toList   shouldBe List(0)
      fibs.take(2)        .toList   shouldBe List(0,1)
      fibs.take(3)        .toList   shouldBe List(0,1,1)
      fibs.take(4)        .toList   shouldBe List(0,1,1,2)
      fibs.take(5)        .toList   shouldBe List(0,1,1,2,3)
      fibs.take(6)        .toList   shouldBe List(0,1,1,2,3,5)
      fibs.take(7)        .toList   shouldBe List(0,1,1,2,3,5,8)
      fibAt(0)                      shouldBe 0
      fibAt(1)                      shouldBe 1
      fibAt(2)                      shouldBe 1
      fibAt(6)                      shouldBe 8
      fibAt(32)                     shouldBe 2178309
      fibAt(100)                    shouldBe BigInt("354224848179261915075")
    }
  }


}
