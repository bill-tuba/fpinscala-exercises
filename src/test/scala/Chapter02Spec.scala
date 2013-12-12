
import fpinscala.datastructures._

import org.scalatest._


class Chapter02Spec extends FlatSpec {

  "An Empty List" should "is the eqivalent of Nil" in {
    assert(List() == Nil)
  }

  "Empty" should "define a List with no data" in {
    assert(List().isEmpty)
    assert(Nil.isEmpty)
  }

  behavior of "A List"
  it should "not be empty when it contains data" in {
    assert(List(1).isEmpty === false)
    assert(List(1, 2).isEmpty === false)
    assert(List(1, 2, 3).isEmpty === false)
  }

  it should "have a head" in {
    assert(List(1).head === 1)
  }

  it should "have a tail" in {
    assert(List(1).tail === Nil)
    assert(List(1, 2).tail === List(2))
    assert(List(1, 2).tail === List(2))
  }

  it can "provide a foldLeft" in {
    assert(List[Int]().foldLeft(0)(_ + _) === 0)
    assert(List[Int]().foldLeft(0)(_ * _) === 0)
    assert(List[Int](1, 2, 3, 4, 5).foldLeft(0)(_ + _) === 15)

  }

  it should "provide a foldRight" in {
    assert(List[Int]().foldRight(0)(_ + _) === 0)
    assert(List[Int]().foldRight(0)(_ * _) === 0)
    assert(List[Int](1, 2, 3, 4, 5).foldLeft(0)(_ + _) === 15)

  }

  it should "throw an exception tailing empty lists" in {

    scala.collection.immutable.List(  List() , Nil).foreach (
      x => intercept[Exception] { x.tail }
    )
  }

  behavior of "Companion Functions of a List"
  it can "fill a List" in {
    assert(List.fill(0, 1) === Nil)
    assert(List.fill(1, 1) === List(1))
    assert(List.fill(5, 1) === List(1, 1, 1, 1, 1))
  }

  it can "sum its numbers" in {
    assert(List.sum(Nil) === 0)
    assert(List.sum(List(1)) === 1)
    assert(List.sum(List(1, 2, 3, 4)) === 10)
  }

  it can "find the product of its numbers" in {
    assert(List.product(Nil) === 1)
    assert(List.product(List(1)) === 1)
    assert(List.product(List(1, 2, 3, 4)) === 24)
  }

  it can "flatten and combine many Lists into one" in {
    assert(List.flatten(List( List(1,2,3), Nil : List[Int]) ) === List(1,2,3))
    assert(List.flatten(List( List(1,2,3), List(4)) ) === List(1,2,3,4))
    assert(List.flatten(List( List(1,2,3), List(4,5)) ) === List(1,2,3,4,5))
    assert(List.flatten(List( List(1), List(2) , List(3)) ) === List(1,2,3))
    assert(List.flatten(List( List(1), List(2,3) , List(4,5)) ) === List(1,2,3,4,5))

  }


}