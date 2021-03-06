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
    list shouldEqual List(-883454042, 1612966641, -549383847, -1151252339, 384748)
  }

  it can "get a double int pair" in {
    val r = RNG(400)
    val rand = randDoubleInt(r)
    val (d, i) = rand._1
    d shouldEqual 0.07166506725906631
    i shouldEqual 1666869822
  }

  it should "fill a list up of rand ints using sequence" in {
    val r = RNG(1)
    val (list, _) = intsWithSeq(5)(r)
    list.sorted shouldEqual List(-883454042, 1612966641, -549383847, -1151252339, 384748).sorted
  }

  it can "generate Positive ints" in {
    val (i, _) = positiveInt(RNG(1))
    // fails on RNG(0)
    i should be > 0
  }

  "roll a die" should "be within range" in{
    (1 to 1000000).foreach( n =>
      rollDie(RNG(n))._1 should  (be > 0 and be < 7)
    )
  }
  def positiveLessThan(n: Int): Rand[Int] =
      flatMap(positiveInt){ i =>
        val mod : Int= i % n
        if (i + (n-1) - mod > 0) rnd => (mod,rnd)
        else positiveLessThan(n)
   }

  def rollDie: Rand[Int] = map( positiveLessThan(6) ) { _ + 1 }

}
