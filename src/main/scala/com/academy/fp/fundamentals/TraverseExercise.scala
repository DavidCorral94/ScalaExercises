package com.academy.fp.fundamentals

import cats.implicits.{catsSyntaxOptionId, none}
import cats.instances.option._
import cats.syntax.traverse._

object TraverseExercise {

  // On this exercise you will have to use traverse or sequence
  // to solve the problem, turning a list of Option values into
  // an optional List, where the list will be defined if and only
  // if all the elements of the list are Some.

  val listA = List(Some(1), Some(2), None)
  val listB = List(None, Some(2), None)
  val listC = List(Some(1), Some(2), Some(3))

  val listOG = List(1, 0, 3)

  def transformElement(i: Int): Option[Int] =
    if (i > 0) i.some else none

  def transformList(l: List[Int]): List[Option[Int]] =
    l.map(transformElement)

  def checkList(l: List[Option[Int]]): Boolean =
    l.forall(_.isDefined)

  def sequenceList(l: List[Option[Int]]): Option[List[Int]] =
    if (checkList(l)) l.sequence else none

  def main(args: Array[String]): Unit = {
    println(sequenceList(listA))
    println(sequenceList(listB))
    println(sequenceList(listC))
    println(sequenceList(transformList(listOG)))
  }

}
