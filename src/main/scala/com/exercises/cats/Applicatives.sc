object Applicatives {

  import cats._
  import cats.implicits._

  Applicative[Option].pure(1) == Option(1)
  Applicative[List].pure(1) == List(1)

  (Applicative[List] compose Applicative[Option]).pure(1) == List(Option(1))


}
