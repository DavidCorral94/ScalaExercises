package com.fd.modelling.day2.session6

import cats.data.NonEmptyList
import com.fd.modelling.day2.session6.DataTypes.UserValidationRules.EmailPredicate
import com.fd.modelling.day2.session6.DataTypes._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineMV
import eu.timepit.refined.types.all.PosLong

import java.time.{LocalDate, Year}

trait Data {
  val starWarsId: PosLong = refineMV[Positive](1)
  val returnOfTheJediId: PosLong = refineMV[Positive](2)
  val theEmpireStrikesBackId: PosLong = refineMV[Positive](3)
  val episodeIThePhantomMenaceId: PosLong = refineMV[Positive](4)

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

  val ewanMcGregor: Person = Person(
    refineMV[Positive](102),
    refineMV[NonEmpty]("Ewan McGregor"),
    LocalDate.of(1971, 3, 31)
  )

  val nataliePortman: Person = Person(
    refineMV[Positive](103),
    refineMV[NonEmpty]("Natalie Portman"),
    LocalDate.of(1981, 6, 9)
  )

  val starWars: Movie = Movie(
    starWarsId,
    refineMV[NonEmpty]("Star Wars"),
    Year.of(1977),
    ScienceFiction,
    Rating.general,
    NonEmptyList.of(
      Performer(harrisonFord, refineMV[NonEmpty]("Han Solo")),
      Performer(carrieFisher, refineMV[NonEmpty]("Princess Leia"))
    )
  )

  val theEmpireStrikesBack: Movie = Movie(
    theEmpireStrikesBackId,
    refineMV[NonEmpty]("The Empire Strikes Back"),
    Year.of(1980),
    ScienceFiction,
    Rating.general,
    NonEmptyList.of(
      Performer(harrisonFord, refineMV[NonEmpty]("Han Solo")),
      Performer(carrieFisher, refineMV[NonEmpty]("Princess Leia"))
    )
  )

  val returnOfTheJedi: Movie = Movie(
    returnOfTheJediId,
    refineMV[NonEmpty]("Return of the Jedi"),
    Year.of(1983),
    ScienceFiction,
    Rating.general,
    NonEmptyList.of(
      Performer(harrisonFord, refineMV[NonEmpty]("Han Solo")),
      Performer(carrieFisher, refineMV[NonEmpty]("Princess Leia"))
    )
  )

  val episodeIThePhantomMenace: Movie = Movie(
    episodeIThePhantomMenaceId,
    refineMV[NonEmpty]("Star Wars: Episode I - The Phantom Menace"),
    Year.of(1999),
    ScienceFiction,
    Rating.general,
    NonEmptyList.of(
      Performer(ewanMcGregor, refineMV[NonEmpty]("Obi-Wan Kenobi")),
      Performer(nataliePortman, refineMV[NonEmpty]("Padmé Amidala"))
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
    Review(user1Id, user1, starWars, 7, None)
  val user2ReviewForStarWars: Review =
    Review(user2Id, user2, starWars, 7, None)

  val user1ReviewForTheEmpireStrikesBack: Review =
    Review(user1Id, user1, theEmpireStrikesBack, 9, None)
  val user2ReviewForTheEmpireStrikesBack: Review =
    Review(user2Id, user2, theEmpireStrikesBack, 7, None)

  val user1ReviewForReturnOfTheJedi: Review =
    Review(user1Id, user1, returnOfTheJedi, 5, None)
  val user2ReviewForReturnOfTheJedi: Review =
    Review(user2Id, user2, returnOfTheJedi, 8, None)

  val user1ReviewForEpisodeIThePhantomMenace: Review =
    Review(user1Id, user1, episodeIThePhantomMenace, 5, None)
  val user2ReviewForEpisodeIThePhantomMenace: Review =
    Review(user2Id, user2, episodeIThePhantomMenace, 6, None)

  val sciFiTopThree = List(theEmpireStrikesBack, starWars, returnOfTheJedi)

}
