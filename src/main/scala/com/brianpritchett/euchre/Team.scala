package com.brianpritchett.euchre

sealed trait Team
case object NorthSouth extends Team
case object EastWest extends Team

object Team {
  def team(player: Player): Team = player match {
    case North => NorthSouth
    case South => NorthSouth
    case East => EastWest
    case West => EastWest
  }
}

