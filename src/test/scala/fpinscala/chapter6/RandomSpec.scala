package fpinscala.chapter6

import org.scalatest.{Matchers, FlatSpec}
import RNG._

class RandomSpec extends FlatSpec with Matchers {
  behavior of "Pseudo Random-Number-Generator P(RNG)"

  it should "cycle state on the next Int calls" in {
    val initialState = RNG(1)
    val (result, state) = initialState.nextInt
    result shouldEqual 384748
    state shouldNot be theSameInstanceAs initialState
  }

  it should "cycle state on next Double giving btw 0..1" in {
    val initialState = RNG(40000)
    val (result, state) = double(initialState)
    result shouldBe 0.8334932754903535
    state shouldNot be theSameInstanceAs initialState
  }

  it can "generate pairs and triples" in {
    val initialState = RNG(40000)
    val ((i, d), _) = intDouble(initialState)
    i shouldEqual -1789913180
    d shouldEqual 0.5754334291328832

    val ((d2, i2), _) = doubleInt(initialState)
    i2 shouldEqual -1235733880
    d2 shouldEqual 0.8334932754903535

    val ((d3, d4, d5), _) = double3(initialState)
    d3 shouldEqual 0.8334932754903535
    d4 shouldEqual 0.5754334291328832
    d5 shouldEqual 0.5315598088929242
  }

  it should "fill a list up of rand ints" in {
    val r = RNG(1)
    val (list, state) = ints(5)(r)
    list.size shouldEqual 5
    state shouldNot be theSameInstanceAs r
    list shouldEqual  List(-883454042, 1612966641, -549383847, -1151252339, 384748)
   }

  it can "get a double int pair" in {
    val r = RNG(400)
    val rand = randDoubleInt(r)
    val (d , i)  = rand._1
    d shouldEqual 0.07166506725906631
    i shouldEqual 1666869822

  }
  it should "have a sandbox" in {
    assert(positiveEven(RNG(2))._1 even_?)
    assert(!positiveEven(RNG(2))._1.odd_?)

    val r = RNG(1)
    val (i, r2) = r.nextInt
    val rand: Rand[Int] = (x: RNG) => x.nextInt

  }

  it should "fill a list up of rand ints using sequence" in {
    val r = RNG(1)
    val (list, state) = ints(5)(r)
    list.size shouldEqual 5
    state shouldNot be theSameInstanceAs r
    list shouldEqual  List(-883454042, 1612966641, -549383847, -1151252339, 384748)
  }

}
