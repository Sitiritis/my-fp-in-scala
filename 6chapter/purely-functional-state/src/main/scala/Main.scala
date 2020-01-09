trait RNG {
  def nextInt: (Int, RNG)

  def randomIntPair: ((Int, Int), RNG) = {
    val (n1, rng1) = nextInt
    val (n2, rng2) = rng1.nextInt
    ((n1, n2), rng2)
  }

  def nonNegativeInt: (Int, RNG) = {
    val (r, rng) = nextInt
    val nnr = r & Int.MaxValue
    (nnr.toInt, rng)
  }

  def nextDoubleBetweenZeroOne: (Double, RNG) = {
    val (nnr, rng) = nonNegativeInt
    val r = nnr.toDouble / Int.MaxValue.toDouble
    (r, rng)
  }

  def nextIntDouble: ((Int, Double), RNG) = {
    val (i, rng1) = nextInt
    val (d, rng2) = rng1.nextDoubleBetweenZeroOne
    ((i, d), rng2)
  }

  def nextDoubleInt: ((Double,Int), RNG) = {
    val (d, rng1) = nextDoubleBetweenZeroOne
    val (i, rng2) = rng1.nextInt
    ((d, i), rng2)
  }

  def nextDouble3: ((Double,Double,Double), RNG) = {
    val (d1, rng1) = nextDoubleBetweenZeroOne
    val (d2, rng2) = rng1.nextDoubleBetweenZeroOne
    val (d3, rng3) = rng2.nextDoubleBetweenZeroOne
    ((d1, d2, d3), rng3)
  }

  def ints(n: Int): (List[Int], RNG) = {
    @annotation.tailrec
    def go(i: Int, l: List[Int], rng: RNG): (List[Int], RNG) =
      if (i < n) {
        val (r, nrng) = rng.nextInt
        go(i + 1, r :: l, nrng)
      }
      else (l, rng)

    go(0, List.empty, this)
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type RandSpecific[+A] = RNG => (A, RNG)
  type Rand[+A] = State.State[RNG, A]

  val int: Rand[Int] = _.nextInt
  def double: Rand[Double] = map(rng => rng.nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble)

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def ints(n: Int): Rand[List[Int]] =
    sequence(List.fill(n)(int))

  def unit[A](a: A): Rand[A] = (a, _)

  def map[A, B](ra: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rngn) = ra(rng)
    (f(a), rngn)
  }

  def mapInTermsOfFlatMap[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def map2InTermsOfFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){ a =>
      map(rb)(f(a, _))
    }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, rngn) = r(rng)
    f(a)(rngn)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def nonNegativeEven: Rand[Int] = map(rng => rng.nonNegativeInt)(i => i - (i % 2))

  // the generated number are skewed towards the numbers that are less than the n % Int.MaxVal
  def nonNegativeLessThanNaive(n: Int): Rand[Int] = map(rng => rng.nonNegativeInt){ _ % n }

  @annotation.tailrec
  def nonNegativeLessThanRetry(n: Int)(rng: RNG): (Int, RNG) = {
    val (i, rngn) = rng.nonNegativeInt
    val mod = i % n
    if ((i + (n - 1) - mod) >= 0) (mod, rngn) else nonNegativeLessThanRetry(n)(rngn)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(rng => rng.nonNegativeInt){ i =>
    val mod = i % n
    if ((i + (n - 1) - mod) >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  def sequenceMy[A](lr: List[Rand[A]]): Rand[List[A]] = rng =>
    lr.foldLeft((List.empty[A], rng)){ (s, x) =>
      val (l, r) = s
      val (rx, rngn) = x(r)
      (rx :: l, rngn)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])){ (ra, rla) => map2(ra, rla){ _ :: _ } }
}

//case class State[S, +A](run: S => (A, S))

object State {
  type State[S, +A] = S => (A, S)

  def unit[S, A](a: A): State[S, A] = (a, _)

  def map[S, A, B](sa: State[S, A])(f: A => B): State[S, B] = s => {
    val (a, sn) = sa(s)
    (f(a), sn)
  }

  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(sa){ a =>
      map(sb){ f(a, _) }
    }

  def flatMap[S, A, B](sa: State[S, A])(f: A => State[S, B]): State[S, B] =
    s => {
      val (a, sn) = sa(s)
      f(a)(sn)
    }

  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
    ls.foldRight(unit[S, List[A]](List.empty[A])){ (sa, sl) => map2(sa, sl){ _ :: _ } }
}

object Main {
  import RNG._
  import State._

  def rollDie: Rand[Int] = RNG.map(nonNegativeLessThan(6)){ _ + 1 }

  def main(argv: Array[String]): Unit = {
    // Mediocre impure non-functional approach, containing blasphemous side-effects
//    val rng = new scala.util.Random
//    println(rng.nextDouble)
//    println(rng.nextDouble)
//    println(rng.nextInt)
//    println(rng.nextInt(4))

    // Elite, right, pure, holy & referentially transparent functional approach
    val rng1 = SimpleRNG(42)
    println(rng1)
    val (n1, rng2) = rng1.nextInt
    println(n1)
    println(rng2)
    val (n2, rng3) = rng2.nextInt
    println(n2)
    println(rng3)
    println(rng3.nextInt._1)

    println("\nnonNegativeInt")
    println(rng1.nonNegativeInt._1)
    println(rng2.nonNegativeInt._1)
    println(rng3.nonNegativeInt._1)

    println("\nnextDoubleBetweenZeroOne")
    println(rng1.nextDoubleBetweenZeroOne._1)
    println(rng2.nextDoubleBetweenZeroOne._1)
    println(rng3.nextDoubleBetweenZeroOne._1)

    println("\ndouble")
    println(double(rng1)._1)
    println(double(rng2)._1)
    println(double(rng3)._1)


    println("\nnextIntDouble")
    println(rng1.nextIntDouble._1)
    println("randIntDouble")
    println(randIntDouble(rng1)._1)
    println("\nnextDoubleInt")
    println(rng1.nextDoubleInt._1)
    println("randDoubleInt")
    println(randDoubleInt(rng1)._1)
    println("\nnextDouble3")
    println(rng1.nextDouble3._1)

    println("\nints")
    println(rng1.ints(10)._1)
    println("\nints (using sequence)")
    println(ints(10)(rng1)._1)

    println("\nsequence")
    println(sequence(List[Rand[Int]](
      rng => rng.nextInt,
      rng => rng.nextInt,
      rng => rng.nextInt,
    ))(rng1)._1)

//    val test: Rand[Int] = nonNegativeLessThanRetry(10)
    println("\nnonNegativeLessThan")
    println(nonNegativeLessThan(11)(rng1)._1)

    println("\ngeneral map")
    println(State.map[RNG, Int, String](_.nextInt){ _.toString }(rng1)._1)
  }
}
