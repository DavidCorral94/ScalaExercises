import cats.{Monoid, Semigroup}
import cats.implicits.{
  catsKernelStdSemilatticeForSet,
  catsSyntaxOptionId,
  catsSyntaxSemigroup
}

object Chapter2_3 {
  // Exercise

  implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = true
    def combine(x: Boolean, y: Boolean) = x && y
  }

  implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = false
    def combine(x: Boolean, y: Boolean) = x || y
  }

  implicit val booleanEitherMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def empty = false
      def combine(a: Boolean, b: Boolean) = (a && !b) || (!a && b)
    }

  implicit val booleanNorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = true
    def combine(x: Boolean, y: Boolean) = (!x || y) && (x || !y)
  }

}

object Chapter2_4 {
  implicit def unionSet[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def empty = Set.empty[A]

      def combine(x: Set[A], y: Set[A]) = x union y
    }

  implicit def intersectSet[A]: Semigroup[Set[A]] =
    new Semigroup[Set[A]] {
      def combine(x: Set[A], y: Set[A]) = x intersect y
    }

  implicit def diffSet[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def empty = Set.empty[A]

      def combine(x: Set[A], y: Set[A]) = (x diff y) union (x diff y)

    }

  val s1 = Set(1, 2, 3, 4)
  val s2 = Set(1, 2, 5, 6)

  val intSetMonoid = Monoid[Set[Int]]
  intSetMonoid.combine(s1, s2)
  intersectSet[Int].combine(s1, s2)
}

object Chapter2_5 {
  def add(items: List[Int]): Int =
    items.sum

  def add[A](items: List[A])(implicit m: Monoid[A]): A = {
    items.foldLeft(m.empty)(_ |+| _)
  }

  add[Option[Int]](List(None, Some(2)))

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(0, 0)

    override def combine(x: Order, y: Order) =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  add[Order](List(Order(1,1),Order(2,2)))
}
