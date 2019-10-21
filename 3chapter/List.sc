sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def copy[A](l: List[A]): List[A] =
    foldRight(l, Nil: List[A])(Cons(_, _))

  def reverse[A](l: List[A]) =
    foldLeft(l, Nil: List[A])((xs, x) => Cons(x, xs))

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def size[A](l: List[A]): Int =
    foldRight(l, 0)((_, cs) => cs + 1)

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def append_foldLeft[A](l: List[A], r: List[A]): List[A] =
    foldLeft(reverse(l), r)((xs, x) => Cons(x, xs)) // same as foldRightViaFoldLeft

  def append_foldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  @scala.annotation.tailrec
  def drop[A](as: List[A], n: Int): List[A] =
  {
    if (n > 0)
      as match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    else
      as
  }

  @scala.annotation.tailrec
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => as
  }

  def setHead[A](as: List[A], h: A): List[A] = as match {
    case Nil => List(h)
    case Cons(_, xs) => Cons(h, xs)
  }

  @scala.annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldLeftViaFoldRight_1[A,B](as: List[A], outerIdentity: B)(combiner: (B, A) => B): B = {
    type BtoB = B => B

    val innterIdentity: BtoB = (b: B) => b

    val combinerDelayer: (A, BtoB) => BtoB =
      (a: A, delayFunc: BtoB) => (b: B) => delayFunc(combiner(b, a))

    val go: BtoB = foldRight(as, innterIdentity)(combinerDelayer)

    go(outerIdentity)
  }

  def union[A](u: List[List[A]]): List[A] =
    foldLeft(u, Nil: List[A])(append_foldLeft)

  def sumFoldRight(l: List[Int]) =
    foldRight(l, 0)(_ + _)

  def prodFoldRight(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  def sumFoldLeft(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def prodFoldLeft(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)


  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil: List[B])((x, xs) => Cons(f(x), xs))

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    union(map(l)(f))

  def mapIncrement(l: List[Int]): List[Int] =
    map(l)(_ + 1)

  def mapDoublesToString(l: List[Double]): List[String] =
    map(l)(_.toString)


  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(l, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)((x) => if (f(x)) Cons(x, Nil) else Nil: List[A])


  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = {
    def go(as: List[A], bs: List[B], res: List[C]): List[C] = {
      (as, bs) match {
        case (Nil, _) => res
        case (_, Nil) => res
        case (Cons(x, xs), Cons(h, t)) => go(xs, t, Cons(f(x, h), res))
      }
    }

    reverse(go(l, r, Nil: List[C]))
  }

  def addPairwise(l: List[Int], r: List[Int]): List[Int] =
    zipWith(l, r)(_ + _)


  @annotation.tailrec
  def hasPrefix[A](l: List[A], prefix: List[A]): Boolean =
    (l, prefix) match {
      case (_, Nil) => true
      case (Cons(h, t), Cons(x, xs)) if h == x => hasPrefix(t, xs)
      case _ => false
    }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil => sub == Nil
      case _ if hasPrefix(sub, sup) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }
}

val a = List(1, 2, 3, 4, 5)
