package com.propertybased.testing.section4

import com.propertybased.testing.section4.DataTypes._
import com.propertybased.testing.section4.Generators._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class DomainSpec
    extends AnyFunSuite
    with Matchers
    with ScalaCheckDrivenPropertyChecks {

  test("Create a random team") {
    forAll { t: Team =>
      assert(t.yearFounded > 1900 && t.yearFounded < 2021)
    }
  }

  test("Test winner match") {
    forAll { g: Game =>
      if (g.homeScore > g.awayScore)
        assert(g.winner.get == g.homeTeam)
      else if (g.homeScore < g.awayScore)
        assert(g.winner.get == g.awayTeam)
      else
        assert(g.winner.isEmpty)
    }
  }

}
