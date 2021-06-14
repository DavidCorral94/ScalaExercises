package com.fd.modelling.day2.session4

import cats.data.EitherNec
import cats.syntax.either._
import cats.syntax.parallel._
import eu.timepit.refined._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import com.fd.modelling.day2.session4.Errors._

import java.time.LocalDate

object Data {
  case class Person(id: PosLong, name: NonEmptyString, birthday: LocalDate)
  object Person {
    def create(
        id: Long,
        name: String,
        birthday: LocalDate
    ): EitherNec[DomainError, Person] = {
      (
        refineV[Positive](id)
          .leftMap(message => NonPositiveValueProvided("id", message))
          .toEitherNec,
        refineV[NonEmpty](name)
          .leftMap(message => EmptyValueProvided("name", message))
          .toEitherNec
      ).parMapN((id, name) => Person(id, name, birthday))
    }
  }

  case class Performer(person: Person, character: NonEmptyString)
  object Performer {
    def create(
        person: Person,
        character: String
    ): EitherNec[DomainError, Performer] = {
      refineV[NonEmpty](character)
        .leftMap(message => EmptyValueProvided("character", message))
        .toEitherNec
        .map(character => Performer(person, character))
    }
  }
}

object MainPerson {

  def main(args: Array[String]): Unit = {
    import Data._
    val p = Person.create(1L, "David", LocalDate.now())
    println(p)

    val per = p match {
      case Right(person) => Some(Performer.create(person, "Jack Sparrow"))
      case Left(e)          => println(e); None
    }
    println(per)
  }

}
