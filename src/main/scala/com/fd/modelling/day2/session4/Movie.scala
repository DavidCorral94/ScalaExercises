package com.fd.modelling.day2.session4

import cats.data.NonEmptyList
import com.fd.modelling.day2.session4.Data.{Performer, Person}
import com.fd.modelling.day2.session4.Errors._
import eu.timepit.refined.refineV
import cats.data.EitherNec
import cats.implicits.catsSyntaxTuple3Parallel
import cats.syntax.either._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString

import java.time.{LocalDate, Year}

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
    ).parMapN((id, title, cast) => Movie(id, title, year, genre, rating, cast))
  }
}

object MainMovie {
  def main(args: Array[String]): Unit = {
/*
    val actors = List(
      Performer.create(
        Person.create(1L, "David Corral", LocalDate.now()).getOrElse(None),
        "Dr. Corral"
      ),
      Performer.create(
        Person.create(2L, "Marie Doe", LocalDate.now()).getOrElse(None),
        "Dr. Marie"
      )
    )
    val maybeMovie = Movie.create(
      1L,
      "Jurassic World",
      Year.now(),
      Genre.fromString("Sci-Fi").getOrElse(None),
      Rating.pg,
      actors.map(_.getOrElse(None))
    )

    println(maybeMovie)*/
  }
}
