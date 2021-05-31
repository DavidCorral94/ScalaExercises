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
