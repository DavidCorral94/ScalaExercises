import cats.implicits.catsSyntaxOptionId

object HKT {

  // F of Kind * -> *
  trait Foo[F[_]] {
    def apply[A](a: A): F[A]
  }

  lazy val maybeFoo = new Foo[Option] {
    override def apply[A](a: A): Option[A] = Option(a)
  }

  val maybeInt = maybeFoo(6)
  val maybeString = maybeFoo("Hello")
  val maybeList = maybeFoo(List(1, 2, 3))

  // Equivalent ==> Not * -> *, but *
  type F = Option[_]
  val maybe: F = Option(5)

  // Option[_] is of kind *
  // Giving an existing type t, it operates over Option[t]
  // Named as existential types

  // Option is of kind * -> *
  // Any possible A, operates over Option[A]
  // Named as universal types

  type StringOr[A] = Either[String, A]

  lazy val eitherFoo = new Foo[StringOr] {
    override def apply[A](a: A): StringOr[A] = Right(a)
  }

  val rightInt = eitherFoo(5)

  type PartialEither[T] = { type TOr[A] = Either[T, A] }

  def eitherFoov2[T] =
    new Foo[PartialEither[T]#TOr] {
      override def apply[A](a: A): PartialEither[T]#TOr[A] = Right(a)
    }

  val eitherStringInt = eitherFoov2[String](5)
  /*
  def eitherFoov3[T] = new Foo[Either[T, *]] {
    override def apply[A](a: A): Either[T, A] =
      Right(a)
  }

  val eitherStringInt = eitherFoov3[String](5)
   */

  trait Bind[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  def tuplef[F[_], A, B](fa: F[A], fb: F[B])(implicit F: Bind[F]): F[(A, B)] =
    F.flatMap(fa)(a => F.map(fb)((a, _)))

  //val intOptTuple = tuplef(1.some, 2.some)
}

