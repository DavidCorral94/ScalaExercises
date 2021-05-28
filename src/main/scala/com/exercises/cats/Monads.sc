object Monads {

  import cats._
  import cats.implicits._

  Option(Option(1)).flatten == Option(1)

  Option(None).flatten  == None
  List(List(1), List(2, 3)).flatten == List(1, 2, 3)
  Monad[Option].pure(1) == Option(1)
  Applicative[Option].pure(1) == Monad[Option].pure(1)

  Monad[List].flatMap(List(1, 2, 3))(x => List(x, x)) ==
    List(1, 1, 2, 2, 3, 3)

  Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy"))

  Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4))
  Monad[List].ifM(List(true, true, true))(List(1, 2), List(3, 4))
  Monad[List].ifM(List(true))(List(1, 2), List(3, 4))
}
