package com.fd.modelling.day2.session5

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.fd.modelling.day2.session4.Data._
import com.fd.modelling.day2.session4.UserValidationRules.EmailPredicate
import com.fd.modelling.day2.session4._
import eu.timepit.refined._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosLong
import org.scalatest.OptionValues
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, Year}

class ReviewServiceSpec
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

  val user1Id: PosLong = refineMV[Positive](200)
  val user2Id: PosLong = refineMV[Positive](201)

  val user1: User =
    User(
      user1Id,
      refineMV[NonEmpty]("john.doe"),
      refineMV[EmailPredicate]("john.doe@example.io"),
      LocalDate.of(1975, 6, 15)
    )
  val user2: User =
    User(
      user2Id,
      refineMV[NonEmpty]("mary.jane"),
      refineMV[EmailPredicate]("mary.jane@example.io"),
      LocalDate.of(1993, 5, 10)
    )

  val user1ReviewForStarWars: Review =
    Review(user1Id, user1, starWars, 5, None)
  val updatedUser1ReviewForStarWars: Review =
    Review(user1Id, user1, starWars, 6, Some(refineMV[NonEmpty]("Fun!")))
  val user2ReviewForStarWars: Review =
    Review(user2Id, user2, starWars, 8, None)

  val starWarsAverageScore: Double = 6.5

  test("addReview should update an existing score for a given user and movie") {
    val reviews = for {
      service <- ReviewService.createInstance[IO]
      _ <- service.addReview(user1ReviewForStarWars)
      averageScore <- service.getAverageScoreForMovie(starWarsId)
      _ <- service.addReview(updatedUser1ReviewForStarWars)
      updatedAverageScore <- service.getAverageScoreForMovie(starWarsId)
    } yield (averageScore, updatedAverageScore)

    reviews.asserting {
      case (initialScore, updatedScore) =>
        initialScore shouldEqual user1ReviewForStarWars.score
        updatedScore shouldEqual updatedUser1ReviewForStarWars.score
    }
  }

  test("getAverageScoreForMovie should return 0.0 for an empty source") {
    val averageScore = for {
      service <- ReviewService.createInstance[IO]
      averageScore <- service.getAverageScoreForMovie(starWarsId)
    } yield averageScore

    averageScore.asserting(_ shouldEqual 0.0)
  }

  test(
    "getAverageScoreForMovie should return 0.0 for an unknown movie identifier"
  ) {
    val averageScore = for {
      service <- ReviewService.createInstance[IO]
      _ <- service.addReview(user1ReviewForStarWars)
      averageScore <- service.getAverageScoreForMovie(returnOfTheJediId)
    } yield averageScore

    averageScore.asserting(_ shouldEqual 0.0)
  }

  test(
    "getAverageScoreForMovie should return the average score for the given movie"
  ) {
    val averageScore = for {
      service <- ReviewService.createInstance[IO]
      _ <- service.addReview(user1ReviewForStarWars)
      _ <- service.addReview(user2ReviewForStarWars)
      averageScore <- service.getAverageScoreForMovie(starWarsId)
    } yield averageScore

    averageScore.asserting(_ shouldEqual starWarsAverageScore)
  }
}
