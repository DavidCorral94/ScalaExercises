package com.propertybased.testing.section4

object DataTypes {

  case class Team(name: String, managerName: String, yearFounded: Int)

  case class Game(
      homeTeam: Team,
      awayTeam: Team,
      homeScore: Int,
      awayScore: Int
  ) {
    val winner: Option[Team] =
      if (homeScore > awayScore) Some(homeTeam)
      else if (awayScore > homeScore) Some(awayTeam)
      else None
  }

}
