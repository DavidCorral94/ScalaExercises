import cats.effect.IO

object RecapMonad {
  import cats._
  import cats.syntax.all._

  class Repeater[F[_]] {
    def repeatCollect[A](
        n: Int
    )(fa: F[A])(implicit F: Monad[F]): F[Vector[A]] = {
      def loop(n: Int, acc: Vector[A]): F[Vector[A]] = {
        if (n <= 0) F.pure(acc)
        else fa.flatMap(a => loop(n - 1, acc :+ a))
      }

      loop(n, Vector.empty[A])
    }

  }

  val repeaterList = new Repeater[List]
  repeaterList.repeatCollect(2)(List(1))

  val repeaterOption = new Repeater[Option]
  repeaterOption.repeatCollect(5)(Some(10))

  val repeaterIO = new Repeater[IO]
  repeaterIO.repeatCollect(10)(IO.pure(10))

  case class Account[A](amount: A)

  implicit def monadOption: Monad[Account] =
    new Monad[Account] {
      override def flatMap[A, B](fa: Account[A])(f: A => Account[B]) =
        f(fa.amount)

      override def tailRecM[A, B](a: A)(f: A => Account[Either[A, B]]) = ???

      override def pure[A](x: A) = Account(x)
    }

  val repeaterAccount = new Repeater[Account]
  repeaterAccount.repeatCollect(10)(Account("10"))
}
