package fpinscala.chapter2.trees

import org.scalatest.{ShouldMatchers, FlatSpec}
import fpinscala.chapter2.trees

class TreeSpec extends FlatSpec with ShouldMatchers {

  behavior of "A Tree"
  "EXERCISE 25:" should "Write a function size that counts the number of nodes (leaves and branches) in a tree." in {
    assert(trees.Nil.size === 0)
    assert(Leaf(1).size === 1)
    assert(Branch(Leaf(1), Leaf(1)).size === 3)
    assert(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))).size === 5)
  }


  "EXERCISE 26:" should "Write a function maximum that returns the maximum element" +
    "in a Tree[Int] . (Note: in Scala, you can use x.max(y) or x max y to" +
    "compute the maximum of two integers x and y .)" in {

    assert(Tree.maximum(Leaf(1)) === 1)
    assert(Tree.maximum(Leaf(2)) === 2)

    assert(Tree.maximum(Branch(Leaf(1), Leaf(2))) === 2)
    assert(Tree.maximum(Branch(Leaf(2), Leaf(1))) === 2)
    assert(Tree.maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) === 3)

    assert(Tree.maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) === 3)

  }

  "EXERCISE 27:" should "Write a function depth that returns the maximum path length from the " +
    "root of a tree to any leaf." in {

    assert(Tree.depth(Leaf(0)) === 0)
    assert(Tree.depth(Branch(Leaf(0), Leaf(0))) === 1)
    assert(Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) === 2)
    assert(Tree.depth(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Branch(Leaf(2), Leaf(3)))) === 3)
  }

  "EXERCISE 28:" should "Write a function map, analogous to the method of the same name on List," +
    "|that modifies each element in a tree with a given function." in {

    assert(Tree.map(Leaf(0)) {
      _ + 1
    } === Leaf(1))
    assert(Tree.map(Branch(Leaf(0), Leaf(0))) {
      _ + 1
    } === Branch(Leaf(1), Leaf(1)))
    assert(Tree.map(Branch(Leaf(1), Leaf(2))) {
      _ + 1
    } === Branch(Leaf(2), Leaf(3)))
    assert(Tree.map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_ * 2) === Branch(Leaf(2), Branch(Leaf(4), Leaf(6))))

  }
  "EXERCISE 29:" should "Generalize size , maximum , depth , and map , writing a new" +
    "function fold that abstracts over their similarities. Re-Implement them in terms of" +
    "this more general function. Can you draw an analogy between this fold function" +
    "and the left and right folds for List ?" in {

    assert(Tree.map(Leaf(1))(_ + 1) === Leaf(2))
    // all other tests should just work
  }
}
