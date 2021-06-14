package com.academy.fd.modelling.day1.session3

class CaseClasses {

  case class Color(red: Int, green: Int, blue: Int) {
    def hexadecimal(): String =
      String.format("#%02x%02x%02x", red, green, blue);

  }

  object Color {
    val red = Color(255, 0, 0)
    val green = Color(0, 255, 0)
    val blue = Color(0, 0, 255)

  }

}
