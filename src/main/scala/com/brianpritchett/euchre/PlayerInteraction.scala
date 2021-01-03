package com.brianpritchett.euchre

trait TrumpChoice
case object Pass extends TrumpChoice
case class Pick(suit: Suit) extends TrumpChoice

trait PlayerInteraction[F[_]] {
  def askToCallTrumpFirstRound(player: Player, hand: Hand, shown: Card): F[TrumpChoice]
  def askToCallTrumpSecondRound(player: Player, hand: Hand, shown: Card): F[TrumpChoice]
  def askForDiscard(dealer: Player, shown: Card, hand: Hand): F[Hand]
  def playCard(up: Player, hand: Hand, alreadyPlayed: Map[Player, Card], trump: Suit, lead: Option[Suit]): F[Card]
}

object PlayerInteraction {
  def apply[F[_]](implicit PI: PlayerInteraction[F]): PlayerInteraction[F] = PI
}
