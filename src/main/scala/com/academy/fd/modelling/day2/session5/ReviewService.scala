package com.academy.fd.modelling.day2.session5

import cats.effect.Sync
import cats.Functor
import cats.syntax.functor._
import cats.effect.concurrent.Ref
import com.academy.fd.modelling.day2.session4.Review
import eu.timepit.refined.types.numeric.PosLong

trait ReviewService[F[_]] {
  def addReview(review: Review): F[Unit]
  def getAverageScoreForMovie(id: PosLong): F[Double]
}

object ReviewService {
  implicit def createInstance[F[_]: Sync]: F[ReviewService[F]] =
    Ref
      .of[F, List[Review]](List.empty)
      .map(ref => new ReviewServiceImpl[F](ref))
}

class ReviewServiceImpl[F[_]: Functor](reviews: Ref[F, List[Review]])
    extends ReviewService[F] {
  override def addReview(review: Review): F[Unit] =
    reviews.modify(list => {
      if (!list.exists(_.id == review.id))
        (review :: list, ())
      else
        (review :: list.filterNot(_.id == review.id), ())
    })

  override def getAverageScoreForMovie(id: PosLong): F[Double] =
    reviews.get.map(list =>
      if (list.isEmpty) {
        0.0
      } else {
        list
          .filter(_.movie.id == id)
          .foldRight(0)((review, acc) =>
            review.score + acc
          ) / list.size.toDouble
      }
    )
}
