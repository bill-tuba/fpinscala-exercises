package fpinscala.chapter6

import org.scalatest.{Matchers, FlatSpec}
import RNG._
class RandomSpec extends FlatSpec with Matchers{
  behavior of "Pseudo Random-Number-Generator P(RNG)"

  implicit def pimpMyInt(n : Int) = new {
    def even_? = (n & 1) == 0
    def odd_?  = ! even_?
  }

  it should "cycle state on the next Int calls" in{
    val initialState= RNG(1)
    val (result, state) = initialState.nextInt
    result shouldEqual 384748
    state  shouldNot be theSameInstanceAs initialState
  }

  it should "cycle state on next Double giving btw 0..1" in{
    val initialState= RNG(40000)
    val (result, state) =double(initialState)
    result shouldBe  0.8334932754903535
    state  shouldNot be theSameInstanceAs initialState
  }

  it can "generate pairs and triples" in {
    val initialState= RNG(40000)
    val ((i,d), _) = intDouble(initialState)
    i shouldEqual -1789913180
    d shouldEqual  0.5754334291328832

    val ((d2,i2), _) = doubleInt(initialState)
    i2 shouldEqual  -1789913180
    d2 shouldEqual  0.5754334291328832

    val ((d3,d4,d5), _) = double3(initialState)
    d3 shouldEqual  0.8334932754903535
    d4 shouldEqual  0.5754334291328832
    d5 shouldEqual  0.5315598088929242
  }

  it should "fill a list up of rand ints" in {
    val r = RNG(1)
    val (list, state) = ints(5)(r)
    list.size shouldEqual 5
    state  shouldNot be theSameInstanceAs r
  }

  it should "poop" in {
    assert ( positiveEven(RNG(2))._1 even_?)
    assert (!positiveEven(RNG(2))._1.odd_? )
  }
}