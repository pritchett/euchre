package com.brianpritchett.euchre

import cats.kernel.Eq
import cats.Applicative
import cats.implicits._
import scala.collection.mutable.ListBuffer

sealed trait Suit
case object Hearts extends Suit
case object Diamonds extends Suit
case object Spades extends Suit
case object Clubs extends Suit

sealed trait Rank
case object Ace extends Rank
case object King extends Rank
case object Queen extends Rank
case object Jack extends Rank
case object Ten extends Rank
case object Nine extends Rank

case class Card(rank: Rank, suit: Suit) {
  override def toString = s"$rank of $suit"
}

object Card {
  implicit val cardEq: Eq[Card] = Eq.fromUniversalEquals

  def isLeftBower(trump: Suit, c: Card) =
    c.rank === Jack && c.suit === Suit.sameColor(trump)

}

object Rank {
  implicit val rankEq: Eq[Rank] = Eq.fromUniversalEquals
}

object Suit {
  implicit val suitEq: Eq[Suit] = Eq.fromUniversalEquals

  def sameColor(suit: Suit) = suit match {
    case Hearts   => Diamonds
    case Diamonds => Hearts
    case Spades   => Clubs
    case Clubs    => Spades
  }
}

object Deck {

  def shuffle[F[_]](
      deck: Deck
  )(implicit A: Applicative[F], R: Random[F]): F[Deck] = {
    import A._
    import R._

    val buffer = pure(new ListBuffer[Card])

    val insert = (build: ListBuffer[Card], i: Int, c: Card) => {
      build.insert(i.abs % (build.size + 1), c)
      build
    }

    val insertF = (build: F[ListBuffer[Card]], c: Card) =>
      (build, nextInt).mapN(insert(_, _, c))

    deck.foldLeft(buffer)(insertF).map(_.toList)

  }

}
