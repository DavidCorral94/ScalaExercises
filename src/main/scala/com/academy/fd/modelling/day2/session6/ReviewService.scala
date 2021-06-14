package com.academy.fd.modelling.day2.session6

import cats.effect.Sync
import com.academy.fd.modelling.day2.session6.DataTypes.Review
import eu.timepit.refined.types.numeric.PosLong
import cats.syntax.functor._

import scala.language.implicitConversions

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

  def apply[F[_]](implicit F: ReviewService[F]): ReviewService[F] = F

  implicit def instance[F[_]: Sync](db: DatabaseService[F]): ReviewService[F] =
    new ReviewService[F] {
      override def addReview(review: Review): F[Unit] =
        db.insertReview(review)

      override def getAverageScoreForMovie(id: PosLong): F[Double] = {
        val reviews: F[List[Review]] = db.selectReviewsByMovie(id)
        reviews.map(list => {
          list.size match {
            case 0 => 0.0
            case _ =>
              list.foldRight(0.0)((r, acc) =>
                acc + r.score
              ) / list.size.toDouble
          }
        })
      }

    }
}
