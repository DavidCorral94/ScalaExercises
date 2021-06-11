package com.propertybased.testing.section3

import com.propertybased.testing.section3.DiceGen.dieArb
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class DiceSpec
    extends AnyFunSuite
    with Matchers
    with ScalaCheckDrivenPropertyChecks {

  test("Two dices should be equal") {
    forAll { d: Dice =>
      Dice.pairRolled(d, d) shouldBe true
    }
  }

  test("pairRolled works when not paired dices") {
    forAll { d1: Dice =>
      forAll(
        Gen.oneOf(List(One, Two, Three, Four, Five, Six).filterNot(_ == d1))
      ) { d2: Dice =>
        Dice.pairRolled(d1, d2) shouldBe false
      }
    }
  }

}
