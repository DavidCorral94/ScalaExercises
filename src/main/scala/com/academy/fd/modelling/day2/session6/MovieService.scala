package com.academy.fd.modelling.day2.session6

import cats.effect.Sync
import com.academy.fd.modelling.day2.session6.DataTypes._
import eu.timepit.refined.types.numeric.PosLong

import scala.language.implicitConversions

/**
  * This is an alternative to the exercises of session 4 and 5
  * -----------
  * Here we define the algebra and interpreter using tagless final
  */

trait MovieService[F[_]] {
  def addMovie(movie: Movie): F[Unit]
  def getMovieById(id: PosLong): F[Option[Movie]]
  def findMoviesByGenre(genre: Genre): F[List[Movie]]
}

object MovieService {

  def apply[F[_]](implicit F: MovieService[F]): MovieService[F] = F

  implicit def instance[F[_]: Sync](db: DatabaseService[F]): MovieService[F] =
    new MovieService[F] {
      override def addMovie(movie: Movie): F[Unit] =
        db.insertMovie(movie)

      override def getMovieById(id: PosLong): F[Option[Movie]] =
        db.selectMovie(id)

      override def findMoviesByGenre(genre: Genre): F[List[Movie]] =
        db.selectMoviesByGenre(genre)
    }
}
