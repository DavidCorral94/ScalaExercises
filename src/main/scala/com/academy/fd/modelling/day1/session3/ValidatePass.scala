package com.academy.fd.modelling.day1.session3

import cats.data.{Ior, IorNec, NonEmptyChain => Nec}
import cats.implicits._

object ValidatePass {

  /**
    * Create a function to validate the correctness of a password. The validation rules are:
    *
    * Required: The password should contain at least numbers and letters.
    * Required: The minimum length of the password should be 8.
    * Required: The password should contain uppercase and lowercase letters.
    * Recommended: The password should contain special characters.
    * Recommended: The length of the password should be 12 or higher.
    */

  case class Password(value: String) extends AnyVal

  val specialChars = List('?', '@', '$', '€', 'ñ')

  def validatePasswordLength(password: Password): IorNec[String, Password] = {
    if (password.value.length < 8)
      Nec.one("Password needs, at least, 8 characters of length").leftIor
    else
      password.rightIor
  }

  def validatePasswordCase(password: Password): IorNec[String, Password] = {
    if (!password.value.exists(_.isUpper) && !password.value.exists(_.isLower))
      Nec
        .one(
          "Password needs, at least, combination of upper and lowercase characters"
        )
        .leftIor
    else
      password.rightIor
  }

  def validatePasswordCharacters(
      password: Password
  ): IorNec[String, Password] = {
    if (
      password.value.exists(_.isDigit) &&
      password.value.exists(_.isLetter) &&
      password.value.exists(specialChars.contains(_))
    ) {
      password.rightIor
    } else if (!password.value.exists(specialChars.contains(_)))
      Ior.both(
        Nec.one("Password should have special characteres"),
        password
      )
    else
      Nec
        .one(
          "Password needs letters and digits"
        )
        .leftIor
  }

  def validatePassword(password: Password): IorNec[String, Password] =
    (
      validatePasswordLength(password),
      validatePasswordCase(password),
      validatePasswordCharacters(password)
    ).tupled.as(password)

  def main(args: Array[String]): Unit = {
    println(validatePassword(Password("123aB€5678")))
  }
}
