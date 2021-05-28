object Functors {

  import cats._
  import cats.implicits._

  Functor[List].map(List("a", "b", "c"))(_ + " mapped")
  Functor[Option].map(Option("Hello"))(_.length) == Option(5)

  val lenOption: Option[String] => Option[Int] = Functor[Option].lift(_.length)
  lenOption(Some("abcd")) == Some(4) // Also Option(4)

  val source = List("Cats", "is", "awesome")
  val product = Functor[List].fproduct(source)(_.length).toMap

  product("Cats") == 4
  product("is") == 2
  product.get("awesome").getOrElse(0) == 7

  val listOpt = Functor[List] compose Functor[Option]
  listOpt.map(List(Some(1), None, Some(3)))(_ + 1) == List(Some(2), None, Some(4))
}
