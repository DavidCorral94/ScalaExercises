object Eithers {

  import cats._
  import cats.implicits._

  val right: Either[String, Int] = Either.right(5)
  right.map(_ + 1) == Right(6)

  val left: Either[String, Int] = Either.left("Something went wrong")
  left.map(_ + 1) == Left("Something went wrong")
  left.map(_ + 1).isLeft == true

  right.flatMap(x => Either.right(x + 1)) == Right(6)
  left.flatMap(x => Either.left(x + 1)) == Left("Something went wrong")

  object EitherStyle {
    def parse(s: String): Either[NumberFormatException, Int] =
      if (s.matches("-?[0-9]+")) Either.right(s.toInt)
      else
        Either.left(new NumberFormatException(s"${s} is not a valid integer."))

    def reciprocal(i: Int): Either[IllegalArgumentException, Double] =
      if (i == 0)
        Either.left(
          new IllegalArgumentException("Cannot take reciprocal of 0.")
        )
      else Either.right(1.0 / i)

    def stringify(d: Double): String = d.toString

    def magic(s: String): Either[Exception, String] =
      parse(s).flatMap(reciprocal).map(stringify)
  }

  EitherStyle.parse("10") == Either.right(10)
  EitherStyle.parse("a").isLeft == true

  EitherStyle.magic("10") == Either.right("0.1")

  import EitherStyle._

  val result = magic("2") match {
    case Left(_: NumberFormatException) => "Not a number!"
    case Left(_: IllegalArgumentException) => "Can't take reciprocal of 0!"
    case Left(_) => "Unknown error"
    case Right(result) => s"Got reciprocal: ${result}"
  }

  result.equalsIgnoreCase("Got reciprocal: 0.5")

  object EitherStyleWithAdts {
    sealed abstract class Error
    final case class NotANumber(string: String) extends Error
    final case object NoZeroReciprocal extends Error

    def parse(s: String): Either[Error, Int] =
      if (s.matches("-?[0-9]+")) Either.right(s.toInt)
      else Either.left(NotANumber(s))

    def reciprocal(i: Int): Either[Error, Double] =
      if (i == 0) Either.left(NoZeroReciprocal)
      else Either.right(1.0 / i)

    def stringify(d: Double): String = d.toString

    def magic(s: String): Either[Error, String] =
      parse(s).flatMap(reciprocal).map(stringify)
  }

  import EitherStyleWithAdts._

  val resultWithAdts = EitherStyleWithAdts.magic("2") match {
    case Left(NotANumber(_)) => "Not a number!"
    case Left(NoZeroReciprocal) => "Can't take reciprocal of 0!"
    case Right(result) => s"Got reciprocal: ${result}"
  }

  EitherStyleWithAdts.magic("2d") == Left(NotANumber("2d"))
  EitherStyleWithAdts.magic("0") == Left(NoZeroReciprocal)
  resultWithAdts.equalsIgnoreCase("Got reciprocal: 0.5")

  val rightV: Either[String, Int] = Right(41)
  rightV.map(_ + 1) == Right(42)

  val leftV: Either[String, Int] = Left("Hello")
  leftV.map(_ + 1) ==  Left("Hello")

  val either: Either[NumberFormatException, Int] =
    Either.catchOnly[NumberFormatException]("abc".toInt)

  either.isLeft == true
}
