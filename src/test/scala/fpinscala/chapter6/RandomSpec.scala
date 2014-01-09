package fpinscala.chapter6

import org.scalatest.{Matchers, FlatSpec}

class RandomSpec extends FlatSpec with Matchers{
  behavior of "Pseudo Random-Number-Generator P(RNG)"

  it should "cycle state on the next Int calls" in{
    val initialState= RNG(1)
    val (result, state) = initialState.nextInt
    result shouldEqual 384748
    state  shouldNot be theSameInstanceAs initialState
  }

  it should "cycle state on next Double giving btw 0..1" in{
    val initialState= RNG(40000)
    val (result, state) = initialState.double(initialState)
    result shouldBe  0.8334932754903535
    state  shouldNot be theSameInstanceAs initialState
  }

  it can "generate pairs and triples" in {
    val initialState= RNG(40000)
    val ((i,d), _) = initialState.intDouble(initialState)
    i shouldEqual -1789913180
    d shouldEqual  0.8334932754903535


    val ((d2,i2), _) = initialState.doubleInt(initialState)
    i2 shouldEqual  -1789913180
    d2 shouldEqual  0.8334932754903535

    val ((d3,d4,d5), _) = initialState.double3(initialState)
    d3 shouldEqual  0.8334932754903535
    d4 shouldEqual  0.8334932754903535
    d5 shouldEqual  0.8334932754903535
  }
}
