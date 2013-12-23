
import fpinscala.datastructures._

import org.scalatest._


class ListSpec extends FlatSpec {

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

  "Map" should "apply a function to each element" in {
    assert((Nil:List[Int])    .map(_ + 1)     === Nil         )
    assert(List(1)            .map(_ + 1)     === List(2)     )
    assert(List(1,2,3)        .map(_ + 1)     === List(2,3,4) )
    assert(List(1.0, 2.0, 3.0).map(_.toString)=== List("1.0","2.0","3.0") )

  }

  "With Filter you" should "be able to retain elements matching a predicate" in {
    assert(List(1,2,3,4,5).filter( _ % 2 != 0 )                                 === List(1,3,5) )
    assert(List(1,2,3,4,5).filter( _ % 2 == 0 )                                 === List(2,4) )
    assert(List(1,2,3,4,5).filter2( _ % 2 == 0 )                                === List(2,4) )
    assert(List("1","22","333","4","5").filter( s => s.length < 3 && s != "5")  === List("1","22","4") )
  }

  "With Flat Map you" should "be able to do some damage" in {
    assert( List.flatMap(List(1,2,3))(i => List(i,i)) === List(1,1,2,2,3,3) )
    assert( List.flatMap(List(1,2,3))(i => List(i,i+1)) === List(1,2,2,3,3,4) )

  }
  "Exercise 22 " should "be a function that accepts two lists and constructs a new list by adding corresponding elements." in {
    assert( List(1,2,3).mapAcross(List(4,5,6))(0)( _ + _ ) === List(5,7,9))
  }


  "Lists" should "be able to detect subsequences" in {
    assert( List(1,2,3,4).hasSubsequence(List(1))=== true)
    assert( List(1,2,3,4).hasSubsequence(List(1,2))=== true)
    assert( List(1,2,3,4).hasSubsequence(List(1,2,3))=== true)
    assert( List(1,2,3,4).hasSubsequence(List(2,3))=== true)
    assert( List(1,2,3,4).hasSubsequence(List(4))=== true)
    assert( List(1).hasSubsequence(List(4))=== false)
    assert( Nil.hasSubsequence(List(4))=== false)
    assert( List(1).hasSubsequence(Nil)=== true)
    assert(List[Int]().hasSubsequence(Nil) ===true)
    assert( Nil.hasSubsequence(Nil) ===true)

  }

  "A zip" should "pull items together" in {

    assert( List(1,3,5).zip( List(2,4,6)) === List(1,2,3,4,5,6))
  }
}