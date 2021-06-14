package com.fd.modelling.day2.session5

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.fd.modelling.day2.session4.{Genre, Movie}

object Program {

  case class MovieWithAvg(movie: Movie, avgScore: Double)

  def getAllMoviesByGenre[F[_]: Monad](
      genre: Genre
  )(implicit movieService: MovieService[F]): F[List[Movie]] =
    movieService.findMoviesByGenre(genre)

  def getMovieWithAvg[F[_]: Monad](movie: Movie)(implicit
      movieService: MovieService[F],
      reviewService: ReviewService[F]
  ): F[MovieWithAvg] =
    reviewService
      .getAverageScoreForMovie(movie.id)
      .map(score => MovieWithAvg(movie, score))

  def topThreeRatedByGenre[F[_]: Monad](genre: Genre)(implicit
      movieService: MovieService[F],
      reviewService: ReviewService[F]
  ): F[List[Movie]] =
    for {
      movies <- movieService.findMoviesByGenre(genre)
      movieWithReview <- movies.traverse(m => getMovieWithAvg(m))
    } yield movieWithReview.sortBy(_.avgScore).map(_.movie).take(3)
}
