package fpinscala.chapter5

import org.scalatest._
import scala.collection.mutable.ListBuffer

class ThunkNGoNuts extends FlatSpec with Matchers {

  it should "evaluate only the truthy case" in {
    assert(if2(b = true, 3, sys.error("uh oh")) === 3)
  }

  it should "evaluate only the falsey case" in {
    a[RuntimeException] should be thrownBy {
      if2(b = false, 3, sys.error("uh oh"))
    }
  }


  val someNum: Int = 1
  it can "evaluate thunks multiple time" in {
    val mutatedTwice = ListBuffer[Int]()
    assert(eagerThunkTwice(true, {
      mutatedTwice += someNum; someNum
    }) === 2)
    assert(mutatedTwice === ListBuffer(someNum, someNum))
  }

  it should "just cache the thunk" in {
    val mutatedOnce = ListBuffer[Int]()
    assert(lazyThunk(true, {
      mutatedOnce += someNum; someNum
    }) === 2)
    assert(mutatedOnce === ListBuffer(someNum))
  }

  def if2[A](b: Boolean, onTrue: => A, onFalse: => A) = if (b) onTrue else onFalse

  def eagerThunkTwice(b: Boolean, thunk: => Int) = if (b) thunk + thunk else 0

  def lazyThunk(b: Boolean, thunk: => Int) = {
    lazy val thinking = thunk;
    if (b) thinking + thinking else 0
  }
}
