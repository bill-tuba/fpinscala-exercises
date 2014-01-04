package fpinscala.chapter5

import org.scalatest._

/**
 * Created by billa on 1/4/14.
 */
class examples extends FlatSpec with Matchers {

  it should "evaluate only the truthy case" in {
    assert(if2(true, 3, sys.error("uh oh")) === 3)
  }

  it should "evaluate only the falsey case" in {
    a[RuntimeException] should be thrownBy {
      if2(false, 3, sys.error("uh oh"))
    }
  }

  def if2[A](boolean: Boolean, onTrue: => A, onFalse: => A) = if (boolean) onTrue else onFalse
}
