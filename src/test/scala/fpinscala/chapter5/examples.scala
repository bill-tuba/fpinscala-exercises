package fpinscala.chapter5

import org.scalatest._

class examples extends FlatSpec with Matchers {

  it should "evaluate only the truthy case" in {
    assert(if2(b = true, 3, sys.error("uh oh")) === 3)
  }

  it should "evaluate only the falsey case" in {
    a[RuntimeException] should be thrownBy {
      if2(b = false, 3, sys.error("uh oh"))
    }
  }

  def if2[A](b: Boolean, onTrue: => A, onFalse: => A) = if (b) onTrue else onFalse
}
