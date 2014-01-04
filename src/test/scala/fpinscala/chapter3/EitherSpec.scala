package fpinscala.chapter3

import org.scalatest.{WordSpec, Matchers}

/**
 * Created with IntelliJ IDEA.
 * User: billa
 * Date: 1/3/14
 * Time: 5:56 PM
 */
class EitherSpec extends WordSpec with Matchers {

  "Map" should {
    "not apply fn if Left" in {
      Left("error").map {
        (x: Int) => x + 1
      } shouldEqual Left("error")
    }
    "apply fn and wrap in Right if Right" in {
      Right(1).map {
        x => x + 1
      } shouldEqual Right(2)
    }
  }
  "FlatMap" should {
    "not apply fn and just return Left" in {
      Left("error").flatMap {
        (x: Int) => Right(x + 1)
      } shouldEqual Left("error")
    }
    "apply fn if Right" in {
      Right(1).flatMap {
        x => Right(x + 1)
      } shouldEqual Right(2)
    }
  }
  "Sequencing" should {
    "result in Right(x) where input is all Rights" in {
      Either.sequence(List(Right(1))) shouldEqual Right(List(1))
      Either.sequence(List(Right(1), Right(2))) shouldEqual Right(List(1, 2))
      Either.sequence(List(Right(1), Right(2), Right(3))) shouldEqual Right(List(1, 2, 3))
    }

    "result in Left where any input is Left" in {
      Either.sequence(List(Left("error"))) shouldEqual Left("error")
      Either.sequence(List(Right(1), Left("error"))) shouldEqual Left("error")
      Either.sequence(List(Right(1), Left("error"), Right(2))) shouldEqual Left("error")
      Either.sequence(List(Right(1), Right(2), Right(3), Left("error"))) shouldEqual Left("error")
      Either.sequence(List(Left("error"), Right(1), Right(2), Right(3))) shouldEqual Left("error")
    }
  }
  "Traverse" should {
    "with any failure results in None" in {
      val sToInt = (s: String) => Either.Try[Exception, Int](s.toInt)
      Either.traverse(Nil)(sToInt) shouldEqual Right(Nil: List[Int])
      Either.traverse(List("1"))(sToInt) shouldEqual Right(List(1))
      Either.traverse(List("1", "2"))(sToInt) shouldEqual Right(List(1, 2))
      Either.traverse(List("1", "y"))(sToInt) shouldBe a[Left[Exception]]
      Either.traverse(List("x", "1"))(sToInt) shouldBe a[Left[Exception]]
      Either.traverse(List("x", "y"))(sToInt) shouldBe a[Left[Exception]]
      Either.traverse(List("1", "2", "3"))(sToInt) shouldEqual Right(List(1, 2, 3))
    }
  }
}
