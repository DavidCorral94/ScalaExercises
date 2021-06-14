package com.academy.fp.fundamentals

import cats.Monoid
import cats.data.Chain

object ExerciseMonoid {

  // On this exercise your task is to implement an instance of Monoid for the Chain datatype
  //
  // Check the documentation of the `Chain` datatype to see how to generate an empty `Chain` and to combine two different `Chain` values

  implicit def monoidForChain[A]: Monoid[Chain[A]] = new Monoid[Chain[A]] {
    override def combine(x: Chain[A], y: Chain[A]): Chain[A] =
      x ++ y

    override def empty: Chain[A] = Chain.empty[A]
  }

  def main(args: Array[String]): Unit = {
    val c1 = Chain(1, 2, 3)
    val c2 = Chain(4, 5, 6)

    println(monoidForChain.combine(c1, c2))
    println(monoidForChain.combine(c1, monoidForChain.empty))
  }

}
