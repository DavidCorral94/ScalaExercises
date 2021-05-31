object Validateds {

  case class ConnectionParams(url: String, port: Int)

  trait Read[A] {
    def read(s: String): Option[A]
  }

  object Read {
    def apply[A](implicit A: Read[A]): Read[A] = A

    implicit val stringRead: Read[String] =
      new Read[String] { def read(s: String): Option[String] = Some(s) }

    implicit val intRead: Read[Int] =
      new Read[Int] {
        def read(s: String): Option[Int] =
          if (s.matches("-?[0-9]+")) Some(s.toInt)
          else None
      }
  }

  sealed abstract class ConfigError
  final case class MissingConfig(field: String) extends ConfigError
  final case class ParseError(field: String) extends ConfigError

  import cats.data.Validated
  import cats.data.Validated.{Invalid, Valid}

  case class Config(map: Map[String, String]) {
    def parse[A: Read](key: String): Validated[ConfigError, A] =
      map.get(key) match {
        case None => Invalid(MissingConfig(key))
        case Some(value) =>
          Read[A].read(value) match {
            case None    => Invalid(ParseError(key))
            case Some(a) => Valid(a)
          }
      }
  }

  def parallelValidate[E, A, B, C](v1: Validated[E, A], v2: Validated[E, B])(
      f: (A, B) => C
  ): Validated[E, C] =
    (v1, v2) match {
      case (Valid(a), Valid(b))       => Valid(f(a, b))
      case (Valid(_), i @ Invalid(_)) => i
      case (i @ Invalid(_), Valid(_)) => i
      case (Invalid(e1), Invalid(e2)) => ???
    }

  val config = Config(Map(("url", "127.0.0.1"), ("port", "1337")))

  val valid = parallelValidate(
    config.parse[String]("url").toValidatedNel,
    config.parse[Int]("port").toValidatedNel
  )(ConnectionParams.apply)

  valid.getOrElse(ConnectionParams("", 0))

  val config2 = Config(Map(("endpoint", "127.0.0.1"), ("port", "not a number")))

  val invalid = parallelValidate(
    config2.parse[String]("url").toValidatedNel,
    config2.parse[Int]("port").toValidatedNel
  )(ConnectionParams.apply)

  import cats.data.Validated
  import cats.data.NonEmptyList

  invalid.isValid == false
  val errors = NonEmptyList(MissingConfig("url"), List(ParseError("port")))

  invalid == Validated.invalid(errors)
}

object OfficialDocs {

  // An example of sequential validation with Either
  sealed trait DomainValidation {
    def errorMessage: String
  }

  case object WrongUsername extends DomainValidation {
    def errorMessage: String = "Wrong username"
  }

  case object WrongPassword extends DomainValidation {
    def errorMessage: String = "Wrong password"
  }

  case object WrongAge extends DomainValidation {
    def errorMessage: String = "Wrong age"
  }

  case class UserData(username: String, password: String, age: Int)

  sealed trait FormValidator {
    def validateUsername(username: String) =
      Either.cond(username.nonEmpty, username, WrongUsername)

    def validatePassword(password: String) =
      Either.cond(password.length > 6, password, WrongPassword)

    def validateAge(age: Int) =
      Either.cond(age >= 18, age, WrongAge)

    def validateForm(username: String, password: String, age: Int) =
      for {
        validUsername <- validateUsername(username)
        validPassword <- validatePassword(password)
        validAge <- validateAge(age)
      } yield UserData(validUsername, validPassword, validAge)
  }

  object FormValidator extends FormValidator

  FormValidator.validateForm("", "", 18)
  FormValidator.validateForm("David", "", 18)
  FormValidator.validateForm("", "LOL", 18)
  FormValidator.validateForm("David", "LOL", 17)
  FormValidator.validateForm("David", "LOL", 18)
  FormValidator.validateForm("David", "LOL123456", 18)

  // Now, update this using the Cat's Validated approach
  import cats.data._
  import cats.data.Validated._
  import cats.implicits._

  sealed trait FormValidatorNec {

    def validateUsername(username: String) =
      if (username.nonEmpty) username.validNec else WrongUsername.invalidNec

    def validatePassword(password: String) =
      if (password.length > 6) password.validNec else WrongPassword.invalidNec

    def validateAge(age: Int) =
      if (age >= 18) age.validNec else WrongAge.invalidNec

    def validateForm(username: String, password: String, age: Int) =
      (validateUsername(username), validatePassword(password), validateAge(age))
        .mapN(UserData)

  }
  object FormValidatorNec extends FormValidatorNec

  FormValidatorNec.validateForm("", "", 18)
  FormValidatorNec.validateForm("David", "", 18)
  FormValidatorNec.validateForm("", "LOL", 18)
  FormValidatorNec.validateForm("David", "LOL", 17)
  FormValidatorNec.validateForm("David", "LOL", 18)
  FormValidatorNec.validateForm("David", "LOL123456", 18)

  val fromVal2Either =
    FormValidatorNec.validateForm("David", "LOL123456", 18).toEither

}
