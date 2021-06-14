package com.fd.modelling.day1.session3

import cats.data.{EitherNec, Validated}
import cats.implicits.catsSyntaxTuple3Parallel

object UnionTypes {

  import SumTypes._

  def componentFromInt[Component](
      value: Int
  )(f: Int => Component): Either[String, Component] =
    if (value >= 0 && value <= 255)
      Right(f(value))
    else
      Left("Invalid value for the component")

  def colorFromIntValuesFailFast(
      red: Int,
      green: Int,
      blue: Int
  ): Either[String, RGB] = {
    for {
      redComponent <- componentFromInt(red)(RedComponent.apply)
      greenComponent <- componentFromInt(green)(GreenComponent.apply)
      blueComponent <- componentFromInt(blue)(BlueComponent.apply)
    } yield RGB(redComponent, greenComponent, blueComponent)
  }

  def colorFromIntValuesAccumulated(
      red: Int,
      green: Int,
      blue: Int
  ): Either[String, RGB] = {
    (
      componentFromInt(red)(RedComponent.apply),
      componentFromInt(green)(GreenComponent.apply),
      componentFromInt(blue)(BlueComponent.apply)
    ).parMapN((redComponent, greenComponent, blueComponent) =>
      RGB(redComponent, greenComponent, blueComponent)
    )
  }
}
