package com.academy.fd.modelling.day2.session6

import cats.Monad
import cats.effect.{IO, Sync}
import cats.effect.concurrent.Ref
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.academy.fd.modelling.day2.session6.DataTypes._

object Program extends Data {

  def populateDB[F[_]: Sync](implicit
      databaseService: DatabaseService[F]
  ): F[Unit] =
    databaseService.insertMovie(starWars) >>
      databaseService.insertMovie(theEmpireStrikesBack) >>
      databaseService.insertMovie(returnOfTheJedi) >>
      databaseService.insertReview(user1ReviewForStarWars) >>
      databaseService.insertReview(user1ReviewForTheEmpireStrikesBack) >>
      databaseService.insertReview(user1ReviewForReturnOfTheJedi) >>
      databaseService.insertReview(user1ReviewForEpisodeIThePhantomMenace) >>
      databaseService.insertReview(user2ReviewForStarWars) >>
      databaseService.insertReview(user2ReviewForTheEmpireStrikesBack) >>
      databaseService.insertReview(user2ReviewForReturnOfTheJedi) >>
      databaseService.insertReview(user2ReviewForEpisodeIThePhantomMenace)

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

  def main(args: Array[String]): Unit = {
    val dbMovie = Ref.of[IO, List[Movie]](Nil).unsafeRunSync()
    val dbReview = Ref.of[IO, List[Review]](Nil).unsafeRunSync()
    implicit val databaseService: DatabaseService[IO] =
      DatabaseService.instance(dbMovie, dbReview)
    implicit val movieService: MovieService[IO] =
      MovieService.instance(databaseService)
    implicit val reviewService: ReviewService[IO] =
      ReviewService.instance(databaseService)

    val sciFiMovies = getAllMoviesByGenre[IO](ScienceFiction).unsafeRunSync()
    sciFiMovies.foreach(println(_))
    val sciFiTop3 =
      Program.topThreeRatedByGenre[IO](ScienceFiction).unsafeRunSync()
    sciFiTop3.foreach(println(_))
  }

}
