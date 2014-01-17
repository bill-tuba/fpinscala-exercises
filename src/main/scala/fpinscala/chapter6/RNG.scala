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

  implicit def intExtensions(n : Int) = new {
    def even_? = (n & 1) == 0
    def odd_?  = ! even_?
    def pos_?  = n > 0
    def neg_?  = !pos_?
  }

  def apply( seed : Int) : RNG =
    new Simple(seed)

  //Pass me a RNG and I'll do some operation (like a state-transition) that passes back a couplet
  //This is an alias for "state-transition" operations
  type Rand[+A] = RNG => (A, RNG)

  //give me A and I'll pass you a fn that takes in a rng and
  // hands you back a Rand
  def unit[A](a: A): Rand[A] =
  rng => (a, rng)

  // I will pass you back a Rand... that
  //  takes RNG a makes the state transition for you
  //    applying fn f to the A part of Rand and return you the new Rand
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => rng => (f(a),rng))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }
  }

  // Returns A Rand Fn that....
  //  If you pass me a RNG I'll do Two state transitions for you
  //      then I'll return a new Rand
  //          where the A part is the result of
  //                applying f to the two A's gathered from the state transitions
  //                and the RNG part is the returned RNG of the 2nd transition

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb){ b=> rng => (f(a, b), rng)})
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
  map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil => unit(Nil)
    case h :: t => map2(h, sequence(t)) {_ :: _}
  }


  val int: Rand[Int] = _.nextInt

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop (c : Int , list : (List[Int], RNG)) : (List[Int], RNG) = (c, list) match {
      case (0, rest )=> rest
      case (cnt, (l,r) ) => val (i,r2): (Int, RNG) = r.nextInt
        loop(cnt -1, ( i :: l , r2))
    }
    loop(count,(List(),rng))
  }

  def intsWithSeq(count: Int)(rng: RNG): (List[Int], RNG)= {
    sequence(List.fill(count){int} )(rng)
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (x, next) = rng.nextInt
    (if (x.neg_?) -(x + 1) else x, next)
  }

  def positiveEven: Rand[Int] =
    map(positiveInt){ i => i ^ 1 & i }

  def double(rng: RNG): (Double, RNG) =
    map(positiveInt) {_.toDouble / Int.MaxValue}(rng)


  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    randIntDouble(rng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    randDoubleInt(rng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1 ,r1) = double(rng)
    val (d2 ,r2) = double(r1)
    val (d3 ,r3) = double(r2)
    ((d1,d2,d3),r3)
  }

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt : Rand[(Double,Int)] =
    both(double,int)
}
