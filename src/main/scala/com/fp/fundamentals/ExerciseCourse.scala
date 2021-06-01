package com.fp.fundamentals

object ExerciseCourse {

  import cats.Show
  import cats.syntax.all._

  trait Stringifier[F[_]] {
    def stringify[A: Show](f: F[A]): String
  }

  object Stringifier {

    def apply[F[_]](implicit S: Stringifier[F]): Stringifier[F] = S

    implicit def eitherStringifier[E: Show]: Stringifier[Either[E, *]] =
      new Stringifier[Either[E, *]] {
        override def stringify[A: Show](f: Either[E, A]): String = {
          f.fold(
            error => s"${error.show} : Left",
            success => s"${success.show} : Right"
          )
        }
      }

    implicit def mapStringifier[K: Show]: Stringifier[Map[K, *]] =
      new Stringifier[Map[K, *]] {
        override def stringify[A: Show](f: Map[K, A]): String = {
          val keyAndValues = f.map {
            case (key: K, value: A) => s""""${key}" : ${value}"""
          }
          s"{\n${keyAndValues.mkString("\n")}\n}"
        }
      }

    implicit class StringifierOps[A: Show, F[_]: Stringifier](toStr: F[A]) {
      def stringify: String = Stringifier[F].stringify(toStr)
    }

  }

  import Stringifier._

  def main(args: Array[String]): Unit = {
    println(3.asRight[String].stringify)
    println(2.asLeft[String].stringify)
    println(Map(("hola", 5), ("adios", 1)).stringify)

  }

}
