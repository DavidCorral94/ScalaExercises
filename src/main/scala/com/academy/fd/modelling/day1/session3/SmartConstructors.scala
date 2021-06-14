package com.academy.fd.modelling.day1.session3

import cats.data.EitherNec
import cats.syntax.either._
import cats.syntax.parallel._

object SmartConstructors {

  sealed trait ColorModel

  case class RedComponent private (value: Int) extends AnyVal

  object RedComponent {
    private def apply(value: Int): RedComponent = new RedComponent(value)

    def fromInt(value: Int): Either[String, RedComponent] =
      Either.cond(
        value >= 0 && value <= 255,
        new RedComponent(value),
        "The given intensity is not valid. The value should be between 0 and 255"
      )
  }

  case class GreenComponent private (value: Int) extends AnyVal

  object GreenComponent {
    private def apply(value: Int): GreenComponent = new GreenComponent(value)

    def fromInt(value: Int): Either[String, GreenComponent] =
      Either.cond(
        value >= 0 && value <= 255,
        new GreenComponent(value),
        "The given intensity is not valid. The value should be between 0 and 255"
      )
  }

  case class BlueComponent private (value: Int) extends AnyVal

  object BlueComponent {
    private def apply(value: Int): BlueComponent = new BlueComponent(value)

    def fromInt(value: Int): Either[String, BlueComponent] =
      Either.cond(
        value >= 0 && value <= 255,
        new BlueComponent(value),
        "The given intensity is not valid. The value should be between 0 and 255"
      )
  }

  case class RGB(red: RedComponent, green: GreenComponent, blue: BlueComponent)
      extends ColorModel

  object RGB {
    private def apply(
        red: RedComponent,
        green: GreenComponent,
        blue: BlueComponent
    ): ColorModel =
      new RGB(red, green, blue)

    def fromIntValues(
        red: Int,
        green: Int,
        blue: Int
    ): EitherNec[String, ColorModel] =
      (
        RedComponent.fromInt(red).toEitherNec,
        GreenComponent.fromInt(green).toEitherNec,
        BlueComponent.fromInt(blue).toEitherNec
      ).parMapN((redComponent, greenComponent, blueComponent) =>
        new RGB(redComponent, greenComponent, blueComponent)
      )
  }

  def main(args: Array[String]): Unit = {
    val badRGB = RGB.fromIntValues(-1, -1, -1)
    println(badRGB)

    val goodRGB = RGB.fromIntValues(1, 1, 1)
    println(goodRGB)
  }

}
