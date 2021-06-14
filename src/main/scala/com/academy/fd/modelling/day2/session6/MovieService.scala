package com.academy.fd.modelling.day2.session6

import cats.effect.Sync
import com.academy.fd.modelling.day2.session6.DataTypes._
import eu.timepit.refined.types.numeric.PosLong

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
  // Simulating a DB
  var db: Map[PosLong, Movie] = Map().empty

  def apply[F[_]](implicit F: MovieService[F]): MovieService[F] = F

  implicit def instance[F[_]: Sync]: MovieService[F] =
    new MovieService[F] {
      override def addMovie(movie: Movie): F[Unit] = {
        db = db + (movie.id -> movie)
        Sync[F].pure(())
      }

      override def getMovieById(id: PosLong): F[Option[Movie]] =
        if (db.contains(id))
          Sync[F].pure(db.get(id))
        else
          Sync[F].pure(None)

      override def findMoviesByGenre(genre: Genre): F[List[Movie]] =
        Sync[F].pure(db.filter(m => m._2.genre == genre).values.toList)
    }
}
