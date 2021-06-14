package com.fd.modelling.day1.session3

object Tuples {
  val red = (255, 0, 0)
  val gree = (0, 255, 0)
  val blue = (0, 0, 255)

  def toHex(color: (Int, Int, Int)): String =
    String.format("#%02x%02x%02x", color._1, color._2, color._3);

  def toHexWithNamedFields(color: (Int, Int, Int)): String = {
    val (red, green, blue) = color

    val redHexCode = f"$red%02x"
    val greenHexCode = f"$green%02x"
    val blueHexCode = f"$blue%02x"

    s"#$redHexCode$greenHexCode$blueHexCode"
  }
}
case class Tuples()
