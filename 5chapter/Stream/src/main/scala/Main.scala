sealed trait Stream[+A] {
  def safeHead: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def safeHeadUsingFoldRight: Option[A] =
    foldRight[Option[A]](None){ (a, _) => Some(a) }

  def safeTail: Option[Stream[A]] = this match {
    case Empty => None
    case Cons(_, t) => Some(t())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def takeUsingUnfold(n: Int): Stream[A] = Stream.unfold(n, this){ state =>
    val (m, s) = state

    s match {
      case Cons(h, t) if m > 0 => Some(h(), (m - 1, t()))
      case _ => Option.empty
    }
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  def takeWhileUsingUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this){
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => Option.empty
  }

  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty){ (a, b) => if (p(a)) Stream.cons(a, b) else Empty }

  @annotation.tailrec
  final def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => if (p(h())) true else t().exists(p)
    case _ => false
  }

  def foldRight[B](u: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(u)(f))
    case _ => u
  }

  def scanRightDirect[B](u: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Cons(h, t) =>
      lazy val rest = t().scanRightDirect(u)(f)
      Stream.cons(f(h(), rest.safeHead.getOrElse(u)), rest)
    case _ => Stream(u)
  }

  def scanRight[B](u: => B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(u)){ (a, s) =>
      Stream.cons(f(a, s.safeHead.getOrElse(u)), s)
    }

  def existsUsingFold(p: A => Boolean): Boolean =
    foldRight(false){ (a, b) => p(a) || b }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true){ (a, b) => p(a) && b }

  def forAllStackSafe(p: A => Boolean): Boolean = {
    @annotation.tailrec
    def go(stream: Stream[A]): Boolean = this match {
      case Cons(h, t) if p(h()) => go(t())
      case Empty => true
      case _ => false
    }

    go(this)
  }

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty){ (a, sb) => Stream.cons(f(a), sb) }

  def mapUsingUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => Option.empty
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty){ (a, sa) => if (p(a)) Stream.cons(a, sa) else sa }

  def filterUsingUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this){
    case Cons(h, t) if p(h()) => Some(h(), t())
    case Cons(_, t) => {
      val res = t().filterUsingUnfold(p)
      res.safeHead.map { (_, res.safeTail.getOrElse(Stream.empty)) }
    }
    case Empty => Option.empty
  }

  def find(p: A => Boolean): Option[A] = filter(p).safeHead

  def appendElement[B >: A](a: => B): Stream[B] =
    foldRight(Stream.cons(a, Stream.empty)){ (aa, as) => Stream.cons(aa, as) }

  def append[B >: A](a: => Stream[B]): Stream[B] =
    foldRight(a){ (aa, sa) => Stream.cons(aa, sa) }

  def flatMap[B](f: A => Stream[B]): Stream[B] = Stream.flatten(map(f))

  def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold(this, other){
      case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
      case (Empty, _) => Option.empty
      case (_, Empty) => Option.empty
    }

  def zipAllWith[B, C](other: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold(this, other){
      case (Cons(ha, ta), Cons(hb, tb)) => Some(f(Some(ha()), Some(hb())), (ta(), tb()))
      case (Empty, Cons(hb, tb)) => Some(f(Option.empty, Some(hb())), (Empty, tb()))
      case (Cons(ha, ta), Empty) => Some(f(Some(ha()), Option.empty), (ta(), Empty))
      case (Empty, Empty) => Option.empty
    }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] =
    zipAllWith(other){ (_, _) }

  def startsWith[B >: A](s: Stream[B]): Boolean = zipAll(s).forAll{ case (oa, ob) => oa == ob }

  def tails: Stream[Stream[A]] = Stream.unfold(this){
    case Cons(_, t) => Some(t(), t())
    case Empty => Option.empty
  }

  def hasSubsequence[B >: A](s: Stream[B]): Boolean = tails exists { _ startsWith s }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Cons[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  //  def constant[A](a: A): Stream[A] = {
  //    lazy val res: Stream[A] = cons(a, res)
  //    res
  //  }

  def constantUsingUnfold[A](a: A): Stream[A] = unfold(a){ _ => Some(a, a) }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  //  def from(n: Int): Stream[Int] = {
  //    lazy val res: Stream[Int] = cons(n, res.map(_ + 1))
  //    res
  //  }

  def fromUsingUnfold(n: Int): Stream[Int] = unfold(n){ i => Some(i, i + 1) }

  def fibs: Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, go(n2, n1 + n2))
    }
    go(0, 1)
  }

  def fibsUsingUnfold: Stream[Int] =
    unfold((0, 1)) { p =>
      Some((
        p._1,
        (p._2, p._1 + p._2)
      ))
    }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def flatten[A](ssa: Stream[Stream[A]]): Stream[A] =
    ssa.foldRight[Stream[A]](empty){ (sa, r) => sa append r }
}

object Main {
  def main(args: Array[String]): Unit = {
    lazy val ones: Stream[Int] = Stream.cons(1, ones)
    val s = Stream(1, 2, 3, 4, 5)
    println(ones.take(5).toList)
    println(ones.exists(_ % 2 != 0))
    println(ones.map(_ + 1).exists(_ % 2 == 0))
    println(ones.takeWhile(_ == 1))
    println(ones.forAll(_ != 1))
//    println(ones.forAllStackSafe(_ == 1)) // does not terminate

    println("Infinite streams in action")
    println(Stream.constant(3).take(3).toList)
    println(Stream.from(5).take(10).toList)
    println(Stream.fibs.take(10).toList)

    println("Unfold in action")
    println(Stream.constantUsingUnfold(3).take(3).toList)
    println(Stream.fromUsingUnfold(5).take(10).toList)
    println(Stream.fibsUsingUnfold.take(10).toList)

    println("Functions using unfold:")
    println("map")
    println(s.mapUsingUnfold{ _ + 10 }.toList)
    println("take")
    println(s.takeUsingUnfold(3).toList)
    println("takeWhile")
    println(s.takeWhileUsingUnfold{ i => i <= 3 || i == 5 }.toList)
    println("filter")
    println(s.filterUsingUnfold{ i => i <= 3 || i == 5 }.toList)
    println("zipWith")
    println(s.zipWith(s){ _ + _ }.toList)
    println("zipAll")
    println(s.zipAll(s).toList)
    println(s.zipAll(Stream(1, 2, 3)).toList)
    println(s.zipAll(Stream(1, 2, 3, 4, 5, 6, 7)).toList)

    println("startWith")
    println(s.startsWith(Stream(1, 2, 3)))
    println(s.startsWith(Stream(7, 6, 5)))
    println("tails")
    println(s.tails.map{ _.toList }.toList)
    println("hasSubsequence")
    println(s hasSubsequence Stream(4, 5))
    println(s hasSubsequence Stream(3, 2))

    println("scanRight")
    println(s.scanRight(0){ _ + _ }.toList)

    println("Checking strictness of scanRight")
    Stream.cons(true, sys.error("oops")).scanRight(){(a, b) =>
        if (a) println("scanRight via foldRight works!") else b
    }.safeHead

    Stream.cons(true, sys.error("oops")).scanRightDirect(){ (a, b) =>
      if (a) println("scanRightDirect works!") else b
    }.safeHead
  }
}
