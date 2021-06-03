import cats.Functor
import cats.implicits.toFunctorOps

import scala.concurrent.{ExecutionContext, Future}

object Chapter3_5 {

  // FUNCTORS

  val func = (x: Int) => x * 2

  val list1 = List(1, 2, 3)
  val functorRes = Functor[List].map(list1)(func)

  val functionLifted =
    Functor[Option].lift(func)

  val list2 = List(Some(1), None, Some(3))
  val functorLiftRes = Functor[List].map(list2)(functionLifted)

  implicit val functorOption: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B) =
      fa.map(f)
  }

  Functor[Option].map(Some(5465))(_ * 2)

  // Functors for future requires the implicit in the definition
  implicit def functorFuture(implicit ec: ExecutionContext): Functor[Future] =
    new Functor[Future] {
      override def map[A, B](fa: Future[A])(f: A => B) =
        fa.map(f)
    }

  // Exercise 3.5.4
  sealed trait Tree[+A]
  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](value: A): Tree[A] =
      Leaf(value)
  }

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B) =
      fa match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(v)             => Leaf(f(v))
      }
  }

  Tree.leaf(100).map(_ * 2)
  Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2)

  Functor[Tree].map(Tree.branch(Tree.leaf(10), Tree.leaf(20)))(_ * 2)

  // CONTRAVARIANT FUNCTORS

  // Contramap Functors
  trait Printable[A] {
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] =
      (value: B) => format(func(value)) // func(value).toString
  }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        s"'${value}'"
    }

  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  format[Boolean](true)

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    new Printable[Box[A]] {
      override def format(value: Box[A]) =
        p.format(value.value)
    }

  format(Box("hello world"))
  format(Box(true))

  // Imap Functors
  trait Codec[A] {
    self =>
    def encode(value: A): String

    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = {
      new Codec[B] {
        def encode(value: B): String =
          self.encode(enc(value))

        def decode(value: String): B =
          dec(self.decode(value))
      }
    }
  }

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }

  implicit val intCodec: Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)

  implicit val booleanCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  // Codec for Box WIP
}
