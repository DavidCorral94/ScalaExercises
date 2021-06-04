object Chapter4_1 {
  // <3 MONADS <3

  // Exercise 4.1.2

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(a => pure(func(a)))
  }
}

object Chapter4_2 {
  // <3 MONADS <3 in Cats
  import cats.Monad
  import cats.instances.option._ // for Monad
  import cats.instances.list._ // for Monad

  val opt1 = Monad[Option].pure(1)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 4))
  val opt3 = Monad[Option].map(opt1)(a => a * 4)

  val list1 = Monad[List].pure(10)
  val list2 = Monad[List].flatMap(list1)(a => List(a, a + 1))
  val list3 = Monad[List].map(list2)(a => a * 2)

  import cats.instances.option._ // for Monad
  import cats.instances.list._ // for Monad
  import cats.syntax.applicative._ // for pure

  // Implicit syntax vs Specific syntax
  1.pure[Option] == Monad[Option].pure(1)
  1.pure[List] == Monad[List].pure(1)

  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
    a.flatMap(x => b.map(y => x * x + y * y))
  }

  sumSquare(Option(1), Option(2))
  sumSquare(List(1, 2), List(2, 3))

  // This is the same but simplified

  def sumSquareSimplier[F[_]: Monad](a: F[Int], b: F[Int]) =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  sumSquareSimplier(Option(1), Option(2))
  sumSquareSimplier(List(1, 2), List(2, 3))

}

object Chapter4_3 {
  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]) =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  // We can't do because 1 and 2 (Int) are not Monads
  //Chapter4_2.sumSquare(1,2)

  // But Cats provides the wonderful Id
  import cats.Id
  sumSquare(1: Id[Int], 2: Id[Int])

  // And we can simplify this even more
  type Id[A] = A
  val a1: Id[Int] = 1
  val a2: Id[Int] = 2
  sumSquare(a1, a2)

  val pureInt = Monad[Id].pure(2)
  val pureList = Monad[Id].pure(List(1, 2, 3))

  for {
    x <- pureInt
    y <- pureList
  } yield y * x

  // Exercise 4.3.1
  // We can do this stuff, directly, because we previously defined
  // type Id[A] = A
  // so substitution does its work :D
  def pure[A](value: A): Id[A] =
    value

  def map[A, B](initial: Id[A])(func: A => B): Id[B] =
    func(initial)

  def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] =
    func(initial)
}
