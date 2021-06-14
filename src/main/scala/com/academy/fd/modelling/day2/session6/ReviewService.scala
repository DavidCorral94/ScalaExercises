package com.fd.modelling.day2.session6

import cats.effect.Sync
import com.fd.modelling.day2.session6.DataTypes.Review
import eu.timepit.refined.types.numeric.PosLong

/**
  * This is an alternative to the exercises of session 4 and 5
  * -----------
  * Here we define the algebra and interpreter using tagless final
  */

trait ReviewService[F[_]] {
  def addReview(review: Review): F[Unit]
  def getAverageScoreForMovie(id: PosLong): F[Double]
}

object ReviewService {
  // Simulating a DB
  var db: Map[PosLong, Review] = Map().empty

  def apply[F[_]](implicit F: ReviewService[F]): ReviewService[F] = F

  implicit def instance[F[_]: Sync]: ReviewService[F] =
    new ReviewService[F] {
      override def addReview(review: Review): F[Unit] = {
        if (db.contains(review.id))
          db = db.removed(review.id) + (review.id -> review)
        else
          db = db + (review.id -> review)
        Sync[F].pure(())
      }

      override def getAverageScoreForMovie(id: PosLong): F[Double] =
        if (!db.exists(r => r._2.id == id))
          Sync[F].pure(0.0)
        else {
          val reviews = db.filter(r => r._2.id == id).values.toList
          Sync[F].pure(
            reviews.foldRight(0.0)((r, acc) =>
              acc + r.score
            ) / reviews.size.toDouble
          )
        }
    }
}
