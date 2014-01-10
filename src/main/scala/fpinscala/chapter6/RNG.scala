package fpinscala.chapter6


trait RNG {
  def nextInt : (Int, RNG)
}

case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = Simple(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {

  val int: Rand[Int] = _.nextInt
  
  def positiveInt(rng: RNG): (Int, RNG) = {
    val (x, next) = rng.nextInt
    (if(x < 0) -(x + 1) else x, next)
  }

  def double(rng: RNG): (Double, RNG) =
    map(positiveInt) {_.toDouble / Int.MaxValue}(rng)

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

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop (c : Int , list : (List[Int], RNG)) : (List[Int], RNG) = (c, list) match {
      case (0, rest )=> rest
      case (cnt, (l,r) ) => val (i,r2): (Int, RNG) = r.nextInt
        loop(cnt -1, ( i :: l , r2))
    }
    loop(count,(List(),rng))
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def positiveEven: Rand[Int] =
    map(positiveInt){ i => i ^ 1 & i }

  def apply( seed : Int) : RNG =
    new Simple(seed)
}