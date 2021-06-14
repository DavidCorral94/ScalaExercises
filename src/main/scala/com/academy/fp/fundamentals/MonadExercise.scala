package com.fp.fundamentals

import cats.Monad

import scala.annotation.tailrec

object MonadExercise {

  implicit def monadOption: Monad[Option] =
    new Monad[Option] {
      override def pure[A](x: A): Option[A] = Option(x)

      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
        fa match {
          case Some(a) => f(a)
          case None    => None
        }

      @tailrec
      override def tailRecM[A, B](
          a: A
      )(f: A => Option[Either[A, B]]): Option[B] =
        f(a) match {
          case Some(Right(b)) => Some(b)
          case Some(Left(a))  => tailRecM(a)(f)
          case None           => None
        }

    }

  def main(args: Array[String]): Unit = {
    val a = monadOption
    val b = a.pure(5)
    val c = a.tailRecM(List(5, 5))(x =>
      if (x.length > 2) Some(Right(x)) else Some(Left(x.concat(List(1))))
    )

    println(c)
  }

}
