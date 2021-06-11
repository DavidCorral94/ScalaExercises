package com.propertybased.testing.section7

object DataTypes {
  sealed abstract class Die(val value: Int)
  case object One extends Die(1)
  case object Two extends Die(2)
  case object Three extends Die(3)
  case object Four extends Die(4)
  case object Five extends Die(5)
  case object Six extends Die(6)

  case class Player(name: String, die: Die)
  object Player {
    def winner(p1: Player, p2: Player): Option[Player] =
      if (p1.die.value > p2.die.value) Some(p1)
      else if (p2.die.value > p1.die.value) Some(p2)
      else None
  }
}
