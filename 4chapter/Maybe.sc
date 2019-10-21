sealed trait Maybe[+T] {
  def apply[A](x: A) = Just(x)
  def empty: Maybe[Nothing] = Empty

  def getOrElse[B >: T](default: => B): B =
    this match {
      case Empty => default
      case Just(x) => x
    }

  def flatMap[B](f: T => Maybe[B]): Maybe[B] =
    this match {
      case Empty => Empty
      case Just(x) => f(x)
    }

  def map[B](f: T => B): Maybe[B] =
    flatMap((x: T) => Just(f(x)))

  def orElse[B >: T](ob: => Maybe[B]): Maybe[B] =
    map { Just(_) } getOrElse ob

  def filter(f: T => Boolean): Maybe[T] =
    flatMap { x: T => if (f(x)) Just(x) else Empty }
}

case class Just[+T](get: T) extends Maybe[T]
case object Empty extends Maybe[Nothing]

object Maybe {
  def lift[A, B](f: A => B): Maybe[A] => Maybe[B] = _ map f

  def map2[A, B, C](a: Maybe[A], b: Maybe[B])(f: (A, B) => C): Maybe[C] =
    a flatMap { af => b map { bf => f(af, bf) } }
}

def sequence_naive[T](x: List[Maybe[T]]): Maybe[List[T]] =
  x.foldRight[Maybe[List[T]]](Just(Nil))((e, r) => Maybe.map2(e, r)(_ :: _))

def traverse[A, B](a: List[A])(f: A => Maybe[B]): Maybe[List[B]] =
  a.foldRight[Maybe[List[B]]](Just(Nil))((e, r) => Maybe.map2(f(e), r)(_ :: _))

def sequence[T](l: List[Maybe[T]]): Maybe[List[T]] =
  traverse(l)(identity)


val abs0: Maybe[Double] => Maybe[Double] = Maybe.lift(math.abs)

def mean(xs: Seq[Double]): Maybe[Double] =
  if (xs.isEmpty) Empty
  else Just(xs.sum / xs.length)

def variance(xs: Seq[Double]): Maybe[Double] =
  mean(xs) flatMap {m => mean(xs.map {x => math.pow(x + m, 2)})}

//mean(List(3.0, 4.0, 5.0))
//
//Some("10") map { x => x.toInt } getOrElse 0
//def add (y: Maybe[Int], x: Maybe[Int]): Maybe[Int] = Maybe.map2(x, y) { (a, b) => a + b }

sequence(List(Just(1), Just(5), Just(7)))
