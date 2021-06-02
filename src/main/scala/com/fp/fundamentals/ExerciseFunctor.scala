package com.fp.fundamentals

import cats.Functor

object ExerciseFunctor {

  // On this exercise your task is to implement an instance of Functor for the Option datatype

  implicit def functorOption: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case Some(value) => Some(f(value))
        case None => None
      }
  }

  def main(args: Array[String]): Unit = {
    val op1 = Some(List(1, 2, 3))
    println(functorOption.map(op1)(e => e.foldRight(0)(_ + _)))
  }

}
