object Traverses {
  import cats.Semigroup
  import cats.data.{NonEmptyList, OneAnd, Validated, ValidatedNel}
  import cats.implicits._

  def parseIntEither(s: String): Either[NumberFormatException, Int] =
    Either.catchOnly[NumberFormatException](s.toInt)

  def parseIntValidated(s: String): ValidatedNel[NumberFormatException, Int] =
    Validated.catchOnly[NumberFormatException](s.toInt).toValidatedNel

  List("1", "2", "3").traverse(parseIntEither) == Right(List(1, 2, 3))
  List("1", "abc", "3").traverse(parseIntEither).isLeft == true

  List("1", "2", "3").traverse(parseIntValidated).isValid

  List(Option(1), Option(2), Option(3)).traverse(identity) == Some(List(1,2,3))
  List(Option(1), None, Option(3)).traverse(identity) == None

  List(Option(1), Option(2), Option(3)).sequence
  List(Option(1), None, Option(3)).sequence

  List(Option(1), Option(2), Option(3)).sequence ==   List(Option(1), Option(2), Option(3)).traverse(identity)

  List(Option(1), Option(2), Option(3)).sequence_ == Some(())
  List(Option(1), None, Option(3)).sequence_ == None
}
