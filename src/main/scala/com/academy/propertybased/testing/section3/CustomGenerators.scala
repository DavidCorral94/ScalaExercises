package com.propertybased.testing.section3

import org.scalacheck.Gen

import java.awt.Color
import java.time.{LocalDate, Year}
import java.time.Month.{
  APRIL,
  AUGUST,
  DECEMBER,
  FEBRUARY,
  JANUARY,
  JULY,
  JUNE,
  MARCH,
  MAY,
  NOVEMBER,
  OCTOBER,
  SEPTEMBER
}

object CustomGenerators {

  val colorGen: Gen[Color] = for {
    r <- Gen.choose(0, 255)
    g <- Gen.choose(0, 255)
    b <- Gen.choose(0, 255)
    a <- Gen.choose(0, 255)
  } yield new Color(r, g, b, a)

  def localDateGen(year: Int): Gen[LocalDate] =
    for {
      month <- Gen.oneOf(
        JANUARY,
        FEBRUARY,
        MARCH,
        APRIL,
        MAY,
        JUNE,
        JULY,
        AUGUST,
        SEPTEMBER,
        OCTOBER,
        NOVEMBER,
        DECEMBER
      )
      dayOfMonth <- Gen.chooseNum(1, month.length(Year.isLeap(year)))
    } yield LocalDate.of(year, month, dayOfMonth)

  def main(args: Array[String]): Unit = {
    val c = colorGen
    println(c.sample)
    val y = localDateGen(2021)
    println(y.sample)
  }
}
