object Foldables {

  import cats._
  import cats.implicits._

  Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) == 6
  Foldable[List].foldLeft(List("a", "b", "c"), "")(_ + _) == "abc"

  val lazyResult =
    Foldable[List].foldRight(List(1, 2, 3), Now(0))((x, rest) =>
      Later(x + rest.value)
    )
  lazyResult.value == 6

  Foldable[List].fold(List("a", "b", "c")) == "abc"
  Foldable[List].fold(List(1, 2, 3)) == 6

  Foldable[List].foldMap(List("a", "b", "c"))(_.length) == 3
  Foldable[List].foldMap(List(1, 2, 3))(_.toString) == "123"

  Foldable[List].foldK(List(List(1, 2), List(3, 4, 5))) == List(1, 2, 3, 4, 5)
  Foldable[List].foldK(List(None, Option("two"), Option("three")))

  Foldable[List].toList(List(1, 2, 3)) == List(1, 2, 3)
  Foldable[Option].toList(Option(42)) == List(42)
  Foldable[Option].toList(None) == List()

  def parseInt(s: String): Option[Int] =
    Either.catchOnly[NumberFormatException](s.toInt).toOption

  Foldable[List].traverse_(List("1", "2", "3"))(parseInt)
  Foldable[List].traverse_(List("a", "b", "c"))(parseInt)

  val FoldableListOption = Foldable[List].compose[Option]
  FoldableListOption.fold(List(Option(1), Option(2), Option(3), Option(4))) == 10
  FoldableListOption.fold(List(Option("1"), Option("2"), None, Option("3"))) == "123"
}
