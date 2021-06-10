package com.fd.modelling.day2.session5

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.fd.modelling.day2.session4.Data.{Performer, Person}
import com.fd.modelling.day2.session4.{Adventure, Movie, Rating, ScienceFiction}
import eu.timepit.refined._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosLong
import org.scalatest.OptionValues
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, Year}

class MovieServiceSpec
    extends AsyncFunSuite
    with AsyncIOSpec
    with Matchers
    with OptionValues {

  val starWarsId: PosLong = refineMV[Positive](1)
  val returnOfTheJediId: PosLong = refineMV[Positive](2)

  val harrisonFord: Person = Person(
    refineMV[Positive](100),
    refineMV[NonEmpty]("Harrison Ford"),
    LocalDate.of(1942, 7, 13)
  )

  val carrieFisher: Person = Person(
    refineMV[Positive](101),
    refineMV[NonEmpty]("Carrie Fisher"),
    LocalDate.of(1956, 10, 21)
  )

  val starWars: Movie = Movie(
    starWarsId,
    refineMV[NonEmpty]("Star Wars"),
    Year.of(1977),
    ScienceFiction,
    Rating.general,
    NonEmptyList.of(
      Performer(harrisonFord, refineMV[NonEmpty]("Han Solo")),
      Performer(carrieFisher, refineMV[NonEmpty]("Princess Leia Organa"))
    )
  )

  test("getMovieById should return None for an empty source") {
    val result = for {
      service <- MovieService.createInstance[IO]
      movie <- service.getMovieById(starWarsId)
    } yield movie

    result.asserting(movie => movie shouldBe None)
  }

  test("getMovieById should return the movie info for the given identifier") {
    val result = for {
      service <- MovieService.createInstance[IO]
      _ <- service.addMovie(starWars)
      movie <- service.getMovieById(starWarsId)
    } yield movie

    result.asserting(movie => movie.value shouldEqual starWars)
  }

  test("getMovieById should return None for an unknown identifier") {
    val result = for {
      service <- MovieService.createInstance[IO]
      _ <- service.addMovie(starWars)
      movie <- service.getMovieById(returnOfTheJediId)
    } yield movie

    result.asserting(movie => movie shouldBe None)
  }

  test("findMoviesByGenre should return an empty list for an empty source") {
    val movies = for {
      service <- MovieService.createInstance[IO]
      movies <- service.findMoviesByGenre(Adventure)
    } yield movies
    movies.asserting(_ shouldBe empty)
  }

  test("findMoviesByGenre should return a list of movies for the given genre") {
    val movies = for {
      service <- MovieService.createInstance[IO]
      _ <- service.addMovie(starWars)
      movies <- service.findMoviesByGenre(ScienceFiction)
    } yield movies
    movies.asserting(_ should contain theSameElementsAs List(starWars))
  }

  test(
    "findMoviesByGenre should return an empty list if there is no match for the given genre"
  ) {
    val movies = for {
      service <- MovieService.createInstance[IO]
      _ <- service.addMovie(starWars)
      movies <- service.findMoviesByGenre(Adventure)
    } yield movies
    movies.asserting(_ shouldBe empty)
  }
}
