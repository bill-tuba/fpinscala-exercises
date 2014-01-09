package fpinscala.chapter6

trait RNG {
  def nextInt : (Int, RNG)
  def double(rng: RNG) : (Double, RNG)
  def intDouble(rng: RNG): ((Int,Double), RNG)
  def doubleInt(rng: RNG): ((Double,Int), RNG)
  def double3(rng: RNG): ((Double,Double,Double), RNG)
  def ints(count: Int)(rng: RNG): (List[Int], RNG)
}

case class Simple(seed: Long) extends RNG {

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (x, next) = nextInt
    (if(x < 0) -(x + 1) else x, next)
  }
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = Simple(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) ={
    val (i, r) = positiveInt(rng)
    (i.toDouble / Int.MaxValue, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, next1) = rng.nextInt
    val (d ,next2) = double(next1)
    ((i,d), next2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i,d),r) = intDouble(rng)
    ((d,i),r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1 ,r1) = double(rng)
      val (d2 ,r2) = double(r1)
      val (d3 ,r3) = double(r2)
      ((d1,d2,d3),r3)
  }
}

object RNG {
  def apply( seed : Int) : RNG =
    new Simple(seed)
}