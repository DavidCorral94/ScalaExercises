package com.fd.modelling.day2.session4

import cats.data.EitherNec
import com.fd.modelling.day2.session4.Errors._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.PosLong
import cats.implicits._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString

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
