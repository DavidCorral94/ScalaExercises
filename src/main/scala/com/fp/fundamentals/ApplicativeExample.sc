import cats.syntax._
import cats.data._
import cats.implicits._

object ApplicativeExample {

  val one = "hello".invalidNec[Int]
  val two = 1.validNec[String]
  val three = "kaboom".invalidNec[Int]

  val res = one.product(two).product(three)

  res.isValid
  res.toEither.left

  (one, two, three).mapN { case (o, tw, th) => (o, tw, th) }


}
