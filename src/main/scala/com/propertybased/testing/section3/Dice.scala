package com.propertybased.testing.section3

import org.scalacheck.{Arbitrary, Gen}

sealed abstract class Dice(value: Int)

case object One extends Dice(1)

case object Two extends Dice(2)

case object Three extends Dice(3)

case object Four extends Dice(4)

case object Five extends Dice(5)

case object Six extends Dice(6)

object Dice {
  def pairRolled(d1: Dice, d2: Dice): Boolean = d1 == d2
}

object DiceGen {
  val genDice: Gen[Dice] = Gen.oneOf(Seq(One, Two, Three, Four, Five, Six))
  implicit val dieArb: Arbitrary[Dice] = Arbitrary(genDice)
}
