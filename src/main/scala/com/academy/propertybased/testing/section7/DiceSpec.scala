package com.propertybased.testing.section7

import com.propertybased.testing.section7.DataTypes._
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class DiceSpec
    extends AnyFunSuite
    with Matchers
    with ScalaCheckDrivenPropertyChecks {

  test("Winner works as expected") {
    val dice: List[Die] = List(One, Two, Three, Four, Five, Six)

    forAll(Gen.choose(1, dice.length - 1)) { index =>
      val (losers, winners) = dice.splitAt(index)

      forAll(Gen.oneOf(losers), Gen.oneOf(winners)) { (l, w) =>
        {
          val losingPlayer = new Player(Gen.alphaStr.sample.get, l)
          val winningPlayer = new Player(Gen.alphaStr.sample.get, w)
          val res = Player.winner(losingPlayer, winningPlayer)

          res shouldBe Some(winningPlayer)
        }
      }
    }
  }

}
