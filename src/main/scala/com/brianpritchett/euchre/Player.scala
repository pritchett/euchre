package com.brianpritchett.euchre

import cats.kernel.Eq

sealed trait Player
case object North extends Player
case object South extends Player
case object East extends Player
case object West extends Player

object Player {
  implicit val playerEq: Eq[Player] = Eq.fromUniversalEquals

  def left(p: Player): Player = p match {
    case North => East
    case South => West
    case East  => South
    case West  => North
  }
}
