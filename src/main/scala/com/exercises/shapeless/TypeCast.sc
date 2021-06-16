object TypeCast {
  import shapeless._
  import syntax.typeable._

  val l: Any = List(Vector("foo", "bar", "baz"), Vector("wibble"))
  l.cast[List[Vector[String]]] == Some(
    List(Vector("foo", "bar", "baz"), Vector("wibble"))
  )
  l.cast[List[Vector[Int]]] == None
  l.cast[List[List[String]]] == None

  val `List[String]` = TypeCase[List[String]]
  val `List[Int]` = TypeCase[List[Int]]
  val list = List(1, 2, 3)

  val result = (list: Any) match {
    case `List[String]`(List(s, _*)) => s.length
    case `List[Int]`(List(i, _*))    => i + 1
  }

  result == 2

}
