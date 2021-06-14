import java.util.UUID

object Chapter7_1 {

  // Exercise 7.1.2

  List(1, 2, 3).foldLeft(List[Int]().empty)((a, b) => b :: a)
  List(1, 2, 3).foldRight(List[Int]().empty)((a, b) => a :: b)

  def foldSum(list: List[Int]): Int =
    list.foldRight(0)(_ + _)

  def foldFilter(list: List[Int], f: Int => Boolean): List[Int] =
    list.foldRight(List[Int]().empty)((a, b) => if (f(a)) a :: b else b)

  def foldMap[A, B](list: List[A])(func: A => B): List[B] =
    list.foldRight(List.empty[B])((a, b) => func(a) :: b)

  def foldFlatMap[A, B](list: List[A])(func: A => List[B]): List[B] =
    list.foldRight(List.empty[B])((a, b) => func(a) ::: b)

  def isEven(a: Int): Boolean = a % 2 == 0

  foldSum(List(1, 2, 3, 4))
  foldFilter(List(1, 2, 3, 4), isEven)
  foldMap(List(1, 2, 3))(_ * 2)
  foldFlatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100))

  // Foldable using Cats
  import cats.Foldable
  import cats.instances.int._ // for Monoid
  import cats.instances.string._ // for Monoid
  import cats.instances.vector._ // for Monoid

  Foldable[List].combineAll(List(1, 2, 3))
  Foldable[List].foldMap(List(1, 2, 3))(_.toString)

  val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
  (Foldable[List] compose Foldable[Vector]).combineAll(ints)

  import cats.syntax.foldable._ // for combineAll and foldMap

  List(1, 2, 3).combineAll
  List(1, 2, 3).foldMap(_.toString)

}

object Chapter7_2 {
  import cats.Applicative
  import cats.syntax.apply._ // for mapN
  import cats.implicits._
  /*
  def listTraverse[F[_]: Applicative, A, B]
  (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }*/

  // Exercise 7.2.2.3
  import cats.data.Validated
  import cats.instances.list._ // for Monoid

  type ErrorsOr[A] = Validated[List[String], A]

  def process(inputs: List[Int]): ErrorsOr[List[Int]] =
    inputs.traverse { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  process(List(2, 4, 6))
  process(List(1, 2, 3))

  // Traverse using Cats
  import cats.Traverse
  import cats.instances.future._ // for Applicative
  import cats.instances.list._ // for Traverse
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration.DurationInt
  import scala.concurrent.{Await, Future}

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60) // just for demonstration

  val totalUptime: Future[List[Int]] =
    Traverse[List].traverse(hostnames)(getUptime)

  Await.result(totalUptime, 1.second)

  val numbers = List(Future(1), Future(2), Future(3))

  val numbers2: Future[List[Int]] =
    Traverse[List].sequence(numbers)

  Await.result(numbers2, 1.second)

  Await.result(hostnames.traverse(getUptime), 1.second)
  Await.result(numbers.sequence, 1.second)

  val usernames = List("DavidC", "PacoR", "RafaP")
  def getId(username: String): Future[UUID] =
    Future(UUID.randomUUID())

  def findIds(username: String): Option[String] =
    if (Math.random() > 0) Option(username.reverse) else None

  val futureOfListOfUUID = usernames.traverse(getId);
  Await.result(futureOfListOfUUID, 1.second)

  val optionOfListOfString = usernames.traverse(findIds)
}
