object Chapter5_2 {
  import cats.data.OptionT

  type ListOption[A] = OptionT[List, A]

  import cats.instances.list._ // for Monad
  import cats.syntax.applicative._ // for pure

  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]

  result1.flatMap { (x: Int) =>
    result2.map { (y: Int) =>
      x + y
    }
  }

  // Building monad stacks
  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  val a = 10.pure[ErrorOrOption]
  val b = 32.pure[ErrorOrOption]
  val c = a.flatMap(x => b.map(y => x + y))


  import scala.concurrent.Future
  import cats.data.{EitherT, OptionT}

  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  import cats.instances.future._ // for Monad
  import scala.concurrent.ExecutionContext.Implicits.global

  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b
}
