sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(x) => x
      case Branch(left, right) => maximum(left) max maximum(right)
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(x) => 0
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(x) => f(x)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 0)((l, r) => 1 + (l max r))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
}

val t: Tree[Int] = Branch(Branch(Leaf(2), Branch(Leaf(3), Leaf(6))), Leaf(10))
