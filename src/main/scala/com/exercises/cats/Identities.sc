object Identities {
  import cats._
  import cats.Functor
  import cats.Monad

  type Id[A] = A

  val x: Id[Int] = 1
  val y: Int = x

  x == y
  x == 1

  val one: Int = 1
  Functor[Id].map(one)(_ + 1) == 2
  Functor[Id].map(x)(_ + 1) == 2

  Monad[Id].map(one)(_ + 1) == 2
  Monad[Id].flatMap(one)(_ + 1) == 2

  val fortytwo: Int = 42
  Comonad[Id].coflatMap(fortytwo)(_ + 1) == 43
}
