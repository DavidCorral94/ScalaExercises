package com.propertybased.testing.section4

import com.propertybased.testing.section4.DataTypes._
import org.scalacheck.{Arbitrary, Gen}

object Generators {

  val teamGen: Gen[Team] = for {
    name <- Arbitrary.arbitrary[String]
    manager <- Arbitrary.arbitrary[String]
    year <- Gen.choose(1900, 2021)
  } yield Team(name, manager, year)

  val gameGen: Gen[Game] = for {
    homeTeam <- teamGen
    awayTeam <- teamGen
    homeScore <- Gen.choose(0, 5)
    awayScore <- Gen.choose(0, 5)
  } yield Game(homeTeam, awayTeam, homeScore, awayScore)

  implicit val teamArb: Arbitrary[Team] = Arbitrary(teamGen)
  implicit val gameArb: Arbitrary[Game] = Arbitrary(gameGen)

}
