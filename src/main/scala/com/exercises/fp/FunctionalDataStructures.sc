object Exercise29 {

  sealed trait Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  size(t) == 5

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(a)      => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  sizeViaFold(t) == size(t)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  maximumViaFold(t) == 3

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((d1, d2) => 1 + (d1 max d2))

  depthViaFold(t)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  mapViaFold(t)(a => a * 2) == Branch(Branch(Leaf(2), Leaf(4)), Leaf(6))

  mapViaFold(t)(_ % 2 == 0)

}
