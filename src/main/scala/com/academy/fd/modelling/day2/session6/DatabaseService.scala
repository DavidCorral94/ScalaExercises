package com.academy.fd.modelling.day2.session6

import cats.effect.Sync
import cats.effect.concurrent.Ref
import com.academy.fd.modelling.day2.session6.DataTypes._
import eu.timepit.refined.types.numeric.PosLong
import cats.syntax.functor._

trait DatabaseService[F[_]] {
  def insertMovie(m: Movie): F[Unit]
  def selectMovie(id: PosLong): F[Option[Movie]]
  def selectMoviesByGenre(g: Genre): F[List[Movie]]
  def insertReview(r: Review): F[Unit]
  def selectReview(id: PosLong): F[Option[Review]]
  def selectReviewsByMovie(id: PosLong): F[List[Review]]
}

object DatabaseService {
  def apply[F[_]: Sync](implicit ev: DatabaseService[F]): DatabaseService[F] =
    ev

  def instance[F[_]: Sync](
      dbMovie: Ref[F, List[Movie]],
      dbReview: Ref[F, List[Review]]
  ): DatabaseService[F] =
    new DatabaseService[F] {
      def insertMovie(u: Movie): F[Unit] =
        dbMovie.modify(movies => (u :: movies, ()))

      def selectMovie(id: PosLong): F[Option[Movie]] =
        dbMovie.get.map(movies => movies.find(_.id == id))

      def selectMoviesByGenre(g: Genre): F[List[Movie]] =
        dbMovie.get.map(movies => movies.filter(_.genre == g))

      def insertReview(r: Review): F[Unit] =
        dbReview.modify(reviews => (r :: reviews, ()))

      def selectReview(id: PosLong): F[Option[Review]] =
        dbReview.get.map(reviews => reviews.find(_.id == id))

      def selectReviewsByMovie(id: PosLong): F[List[Review]] =
        dbReview.get.map(reviews => reviews.filter(_.movie.id == id))
    }
}
