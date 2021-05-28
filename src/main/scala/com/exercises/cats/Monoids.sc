object Monoids {
  import cats._
  import cats.implicits._

  Monoid[String].empty == ""
  Monoid[String].combineAll(List("a", "b", "c")) == "abc"
  Monoid[String].combineAll(List()) == ""

  Monoid[Map[String, Int]]
    .combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3))) ==
    Map("a"->4, "b"->2)

  Monoid[Map[String, Int]].combineAll(List()) == Map()

  val l = List(1, 2, 3, 4, 5)

  l.foldMap(identity) == 15
  l.foldMap(i => i.toString) == "12345"

  implicit def monoidTuple[A: Monoid, B: Monoid]: Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def combine(x: (A, B), y: (A, B)): (A, B) = {
        val (xa, xb) = x
        val (ya, yb) = y
        (Monoid[A].combine(xa, ya), Monoid[B].combine(xb, yb))
      }
      def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)
    }

  l.foldMap(i => (i, i.toString))(monoidTuple) == (15, "12345")

}
