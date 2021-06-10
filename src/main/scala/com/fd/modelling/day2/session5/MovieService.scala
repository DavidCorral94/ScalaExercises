package com.fd.modelling.day2.session5

import cats.Functor
import cats.syntax.functor._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import com.fd.modelling.day2.session4.{Genre, Movie}
import eu.timepit.refined.types.numeric.PosLong

trait MovieService[F[_]] {
  def addMovie(movie: Movie): F[Unit]
  def getMovieById(id: PosLong): F[Option[Movie]]
  def findMoviesByGenre(genre: Genre): F[List[Movie]]
}

object MovieService {
  implicit def createInstance[F[_]: Sync]: F[MovieService[F]] =
    Ref.of[F, List[Movie]](List.empty).map(ref => new MovieServiceImpl[F](ref))
}

class MovieServiceImpl[F[_]: Functor](movies: Ref[F, List[Movie]])
    extends MovieService[F] {

  override def addMovie(movie: Movie): F[Unit] =
    movies.modify(list => (movie :: list, ()))

  override def getMovieById(id: PosLong): F[Option[Movie]] =
    movies.get.map(list => list.find(_.id == id))

  override def findMoviesByGenre(genre: Genre): F[List[Movie]] =
    movies.get.map(list => list.filter(_.genre == genre))
}
