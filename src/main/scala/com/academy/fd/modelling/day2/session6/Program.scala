package com.academy.fd.modelling.day2.session6

import cats.Monad
import cats.effect.IO
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.academy.fd.modelling.day2.session6.DataTypes._

object Program extends Data {

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

  def populateDB(): Unit = {
    MovieService[IO].addMovie(starWars)
    MovieService[IO].addMovie(theEmpireStrikesBack)
    MovieService[IO].addMovie(returnOfTheJedi)

    ReviewService[IO].addReview(user1ReviewForStarWars)
    ReviewService[IO].addReview(user1ReviewForTheEmpireStrikesBack)
    ReviewService[IO].addReview(user1ReviewForReturnOfTheJedi)
    ReviewService[IO].addReview(user1ReviewForEpisodeIThePhantomMenace)

    ReviewService[IO].addReview(user2ReviewForStarWars)
    ReviewService[IO].addReview(user2ReviewForTheEmpireStrikesBack)
    ReviewService[IO].addReview(user2ReviewForReturnOfTheJedi)
    ReviewService[IO].addReview(user2ReviewForEpisodeIThePhantomMenace)
  }

  def main(args: Array[String]): Unit = {
    populateDB()

    val sciFiMovies = getAllMoviesByGenre[IO](ScienceFiction).unsafeRunSync()
    val sciFiTop3 =
      Program.topThreeRatedByGenre[IO](ScienceFiction).unsafeRunSync()
    sciFiTop3.foreach(println(_))
  }

}
