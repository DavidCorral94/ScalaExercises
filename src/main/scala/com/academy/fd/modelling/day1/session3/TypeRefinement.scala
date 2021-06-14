package com.fd.modelling.day1.session3

import cats.data.EitherNec

object TypeRefinement {

  /*
  Define the ComponentRangeValue type as a predicate that only allows values between 0 and 255
  Create Color values for red, green, and blue by providing the literal values of each
  Implement the Color.fromComponentValues method.
  It will only create a Color value if all the passed parameters are valid.
  Otherwise, it will return all the detected errors.
   */

  import cats.data.NonEmptyChain
  import cats.syntax.either._
  import cats.syntax.parallel._
  import eu.timepit.refined._
  import eu.timepit.refined.api.Refined
  import eu.timepit.refined.numeric._

  type ComponentRangeValue = Interval.Closed[0, 255]

  case class RGB(
      red: Int Refined ComponentRangeValue,
      green: Int Refined ComponentRangeValue,
      blue: Int Refined ComponentRangeValue
  )

  object RGB {
    val red: RGB = RGB(
      refineMV[ComponentRangeValue](255),
      refineMV[ComponentRangeValue](0),
      refineMV[ComponentRangeValue](0)
    )
    val green: RGB = RGB(
      refineMV[ComponentRangeValue](0),
      refineMV[ComponentRangeValue](255),
      refineMV[ComponentRangeValue](0)
    )
    val blue: RGB = RGB(
      refineMV[ComponentRangeValue](0),
      refineMV[ComponentRangeValue](0),
      refineMV[ComponentRangeValue](255)
    )

    def fromComponentValues(
        red: Int,
        green: Int,
        blue: Int
    ): EitherNec[String, RGB] = {
      (
        refineV[ComponentRangeValue](red).toEitherNec,
        refineV[ComponentRangeValue](green).toEitherNec,
        refineV[ComponentRangeValue](blue).toEitherNec
      ).parMapN(RGB.apply)
    }
  }

  def main(args: Array[String]): Unit = {
    val invalidRGB = RGB.fromComponentValues(-1, -1, -1)
    println(invalidRGB)

    val validRGB = RGB.fromComponentValues(255, 0, 0)
    println(validRGB)
  }
}
