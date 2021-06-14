package com.fd.modelling.day2.session6

import cats.implicits._
import cats.data.{EitherNec, NonEmptyList}
import com.fd.modelling.day2.session6.DataTypes.UserValidationRules.{
  Email,
  EmailPredicate
}
import com.fd.modelling.day2.session6.Errors._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString

import java.time.temporal.ChronoUnit
import java.time.{LocalDate, Year}

/**
  * This is an alternative to the exercises of session 4 and 5
  * -----------
  * In this object are all the data types used in the exercises
  */

object DataTypes {

  sealed trait Genre
  case object Action extends Genre
  case object Adventure extends Genre
  case object Comedy extends Genre
  case object Drama extends Genre
  case object ScienceFiction extends Genre
  case object Thriller extends Genre

  object Genre {
    def fromString(genre: String): EitherNec[GenreNotSupported, Genre] =
      genre.toLowerCase() match {
        case "action"                     => Action.rightNec
        case "adventure"                  => Adventure.rightNec
        case "comedy"                     => Comedy.rightNec
        case "drama"                      => Drama.rightNec
        case "science fiction" | "sci-fi" => ScienceFiction.rightNec
        case "thriller"                   => Thriller.rightNec
        case _                            => GenreNotSupported(genre).leftNec
      }
  }

  case class Rating private (description: String) extends AnyVal

  object Rating {
    private def apply(description: String): Rating = new Rating(description)

    val general = new Rating(
      "All ages admitted. Nothing that would offend parents for viewing by children."
    )
    val pg = new Rating(
      "Some material may not be suitable for children. Parents urged to give \"parental guidance\"."
    )
    val pg13 = new Rating(
      "Some material may be inappropriate for children under 13. Parents are urged to be cautious."
    )
    val restricted = new Rating(
      "Under 17 requires accompanying parent or adult guardian. Contains some adult material."
    )
    val nc17 = new Rating(
      "No One 17 and Under Admitted. Clearly adult. Young children will not be admitted to watch the film."
    )
  }

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

  case class Movie(
      id: PosLong,
      title: NonEmptyString,
      year: Year,
      genre: Genre,
      rating: Rating,
      cast: NonEmptyList[Performer]
  )
  object Movie {
    def create(
        id: Long,
        title: String,
        year: Year,
        genre: Genre,
        rating: Rating,
        cast: List[Performer]
    ): EitherNec[DomainError, Movie] = {
      (
        refineV[Positive](id)
          .leftMap(message => NonPositiveValueProvided("id", message))
          .toEitherNec,
        refineV[NonEmpty](title)
          .leftMap(message => EmptyValueProvided("title", message))
          .toEitherNec,
        NonEmptyList.fromList(cast).toRight(EmptyCast).toEitherNec
      ).parMapN((id, title, cast) =>
        Movie(id, title, year, genre, rating, cast)
      )
    }
  }

  object UserValidationRules {
    type EmailPredicate = MatchesRegex["""\b[\w\.-]+@[\w\.-]+\.\w{2,4}\b"""]
    type Email = String Refined EmailPredicate
  }
  case class User(
      id: PosLong,
      name: NonEmptyString,
      email: Email,
      birthday: LocalDate
  )
  object User {

    def create(
        id: Long,
        name: String,
        email: String,
        birthday: LocalDate
    ): EitherNec[DomainError, User] =
      (
        refineV[Positive](id)
          .leftMap(message => NonPositiveValueProvided("id", message))
          .toEitherNec,
        refineV[NonEmpty](name)
          .leftMap(message => EmptyValueProvided("name", message))
          .toEitherNec,
        refineV[EmailPredicate](email)
          .leftMap(message => EmailNotValid(message, message))
          .toEitherNec,
        Either
          .cond(
            ChronoUnit.YEARS.between(birthday, LocalDate.now) >= 18,
            birthday,
            UserUnder18(birthday)
          )
          .toEitherNec
      ).parMapN((id, name, email, birthday) => User(id, name, email, birthday))
  }

  case class Review(
      id: PosLong,
      user: User,
      movie: Movie,
      score: Int,
      comment: Option[NonEmptyString]
  )
  object Review {
    def create(
        id: Long,
        user: User,
        movie: Movie,
        score: Int,
        comment: String
    ): EitherNec[DomainError, Review] =
      (
        refineV[Positive](id)
          .leftMap(message => EmptyValueProvided("id", message))
          .toEitherNec,
        Either
          .cond(score >= 0 && score <= 10, score, InvalidScore(score))
          .toEitherNec
      ).parMapN((id, score) =>
        Review(id, user, movie, score, refineV[NonEmpty](comment).toOption)
      )
  }

  case class MovieWithAvg(movie: Movie, avgScore: Double)
}
