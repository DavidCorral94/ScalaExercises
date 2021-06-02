import cats.Applicative
import cats.implicits.catsSyntaxTuple3Semigroupal

object TraverseExample {
  val username: Option[String] = Some("admin")
  val password: Option[String] = Some("1234")
  val url: Option[String] = Some("nsa.com")

  (username, password, url).mapN {
    case (user, pass, web) =>
      ()
  }

  type Id[A] = A
  implicit def idForApplicative[A] =
    new Applicative[Id] {
      def pure[A](a: A): Id[A] = a
      def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] = ff(fa)
    }

}
