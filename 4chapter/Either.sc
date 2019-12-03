sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => f(x)
    case Left(e) => Left(e)
  }

  def mapInTermsOfFlatMap[B](f: A => B): Either[E, B] =
    flatMap { a => Right(f(a)) }

  def orElse[EE >: E, B >: A](o: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => o
  }

  def map2[EE >: E, B, C](o: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for { a <- this; b <- o } yield f(a, b)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

def mean(xs: IndexedSeq[Double]): Either[String, Double] =
  if (xs.isEmpty) Left("Cannot calculate the mean of an empty list")
  else Right(xs.sum / xs.length)

def Try[T](x: => T): Either[Exception, T] =
  try Right(x) catch { case e: Exception => Left(e)}

def safeDiv(x: Int, y: Int): Either[Exception, Int] = Try(x / y)

def sequenceNaive[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
  es.foldRight[Either[E, List[A]]](Right(Nil)) { (a, r) =>
    r.map2(a){ (l, b) => b :: l }
  }

def traverse[A, E, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
  as.foldRight[Either[E, List[B]]](Right(Nil)) { (a, r) =>
    r.map2(f(a)){ (l, b) => b :: l }
  }

def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
  traverse(es)(identity)
