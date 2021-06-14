package com.fd.modelling.day2.session6

import java.time.LocalDate

object Errors {
  sealed abstract class DomainError extends Product with Serializable
  case class GenreNotSupported(givenValue: String) extends DomainError
  case class NonPositiveValueProvided(field: String, message: String)
      extends DomainError
  case class EmptyValueProvided(field: String, message: String)
      extends DomainError
  case object EmptyCast extends DomainError
  case class EmailNotValid(field: String, message: String) extends DomainError
  case class UserUnder18(birthday: LocalDate) extends DomainError
  case class InvalidScore(score: Int) extends DomainError
}
