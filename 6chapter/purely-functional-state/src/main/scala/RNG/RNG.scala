package RNG

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

object RNG {
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
