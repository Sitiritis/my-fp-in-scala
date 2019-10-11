sealed trait Option[+A]
{
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(x => if (f(x)) Some(x) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None else Some(xs.sum / xs.size)

def variance(xs: Seq[Double]): Option[Double] =
 mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
