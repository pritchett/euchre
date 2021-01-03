package com.brianpritchett.euchre

import cats.implicits._
import cats._
import scala.collection.mutable.ListBuffer
import cats.effect.IO
import cats.data.OptionT
import cats.data.StateT

object Game extends App {
  implicit val piio = new PlayerInteraction[IO] {

    override def askToCallTrumpFirstRound(
        player: Player,
        hand: com.brianpritchett.euchre.Hand,
        shown: Card
    ): IO[TrumpChoice] = for {
      _ <- IO.delay(println("\u001b[2J"))
      _ <- IO.delay(println(s"You are $player"))
      _ <- IO.delay(println("Your hand:"))
      _ <- IO.delay(hand.map(println))
      _ <- IO.delay { println(s"$shown is shown. Do you want to pick it? ") }
      answer <- IO.delay { scala.io.StdIn.readBoolean }
    } yield if (answer) Pick(shown.suit) else Pass

    override def askToCallTrumpSecondRound(
        player: Player,
        hand: com.brianpritchett.euchre.Hand,
        shown: Card
    ): IO[TrumpChoice] = for {
      _ <- IO.delay(println("\u001b[2J"))
      _ <- IO.delay(println(s"You are $player"))
      _ <- IO.delay(println("Your hand:"))
      _ <- IO.delay(hand.map(println))
      _ <- IO { println(s"Pick a suit. Cannot choose ${shown.suit}") }
      answer <- IO { scala.io.StdIn.readLine() }
    } yield answer.strip match {
      case "diamonds" => Pick(Diamonds)
      case "hearts"   => Pick(Hearts)
      case "spades"   => Pick(Spades)
      case "clubs"    => Pick(Clubs)
      case _          => Pass
    }

    override def askForDiscard(
        dealer: Player,
        shown: Card,
        hand: com.brianpritchett.euchre.Hand
    ): IO[com.brianpritchett.euchre.Hand] = for {
      _ <- IO.delay(println("\u001b[2J"))
      _ <- IO.delay {
        hand.zipWithIndex.map { case (c, i) => println(s"$i) $c") }
      }
      _ <- IO.delay { println(s"Choose a discard: $hand") }
      num <- IO.delay { scala.io.StdIn.readInt() }
    } yield shown :: hand.filterNot(_ == hand(num))

    override def playCard(
        up: Player,
        hand: Hand,
        alreadyPlayed: Map[Player, Card],
        trump: Suit,
        lead: Option[Suit]
    ): IO[Card] = {
      for {
        _ <- IO.delay(println("\u001b[2J"))
        _ <- IO.delay(println(s"you are: $up"))
        _ <- IO.delay(hand.zipWithIndex.map { case (card, i) =>
          val color =
            if (card.suit === trump || Card.isLeftBower(trump, card))
              scala.io.AnsiColor.GREEN
            else if (lead.contains(card.suit)) scala.io.AnsiColor.YELLOW
            else scala.io.AnsiColor.WHITE
          println(s"$color$i) $card")
        })
        _ <- IO.delay(println(scala.io.AnsiColor.WHITE))
        _ <-
          if (alreadyPlayed.nonEmpty)
            IO.delay(println(s"already played: $alreadyPlayed"))
          else IO.unit
        idx <- IO.delay(scala.io.StdIn.readInt)
      } yield hand(idx)
    }
  }

  implicit val randomIO = new Random[IO] {
    val rand = new scala.util.Random(System.currentTimeMillis())

    override def nextInt: IO[Int] = IO.delay { rand.nextInt() }

  }

  val deck: Deck = for {
    suit <- List(Hearts, Diamonds, Spades, Clubs)
    rank <- List(Nine, Ten, Jack, Queen, King, Ace)
  } yield Card(rank, suit)

  Euchre.loop[IO](North, deck).unsafeRunSync
}

object Euchre {
  import Deck._

  def loop[F[_]: PlayerInteraction: Random](
      dealer: Player,
      deck: Deck
  )(implicit M: Monad[F]): F[Team] = {
    import Player.left
    import M.pure

    def inner(
        dealer: Player,
        deck: Deck,
        nsScore: Int,
        ewScore: Int
    ): F[Team] = {

      def addAndLoop(team: Team, toAdd: Int): F[Team] = team match {
        case NorthSouth =>
          if (nsScore + toAdd >= 10) pure(NorthSouth)
          else inner(left(dealer), deck, nsScore + toAdd, ewScore)
        case EastWest =>
          if (ewScore + toAdd >= 10) pure(EastWest)
          else inner(left(dealer), deck, nsScore, ewScore + toAdd)
      }

      for {
        shuffled <- shuffle[F](deck)
        (hands, leftOver) = deal(shuffled)
        dealt = hands
          .foldLeft((List.empty[(Player, Hand)], dealer)) {
            case ((list, player), hand) =>
              (left(player) -> hand :: list, left(player))
          }
          ._1
          .toMap
        res <- playHand[F](dealt, leftOver, dealer)
        winner <- res match {
          case Some((team, _, false))    => addAndLoop(team, 2)
          case Some((team, false, true)) => addAndLoop(team, 1)
          case Some((team, true, true))  => addAndLoop(team, 2)
          case None                      => inner(left(dealer), deck, nsScore, ewScore)
        }
      } yield winner

    }

    inner(dealer, deck, 0, 0)
  }

  case class RoundState(
      hands: Map[Player, Hand],
      trump: Suit,
      firstPlayed: Option[Suit],
      up: Player,
      alreadyPlayed: Map[Player, Card]
  )

  def playHand[F[_]](
      hands: Map[Player, Hand],
      leftOver: Deck,
      dealer: Player
  )(implicit
      M: Monad[F],
      PI: PlayerInteraction[F]
  ): F[Option[(Team, Boolean, Boolean)]] = {

    import M.pure
    import Player.left
    import Suit.sameColor

    val flippedCard = leftOver.head
    val flippedSuit = flippedCard.suit

    def playRound(implicit PI: PlayerInteraction[F]) = for {
      card1 <- playCard
      card2 <- playCard
      card3 <- playCard
      card4 <- playCard
      winner <- calcRoundWinner
    } yield winner

    def calcRoundWinner = StateT[F, RoundState, Player] { state =>
      val isLeadingSuit = state.firstPlayed.contains _

      def isTrumpSuit(suit: Suit) = suit === state.trump

      def value(card: Card) = card match {
        case Card(Jack, suit) if isTrumpSuit(suit)               => 13
        case Card(Jack, suit) if suit === sameColor(state.trump) => 12
        case Card(Ace, suit) if isTrumpSuit(suit)                => 11
        case Card(King, suit) if isTrumpSuit(suit)               => 10
        case Card(Queen, suit) if isTrumpSuit(suit)              => 9
        case Card(Ten, suit) if isTrumpSuit(suit)                => 8
        case Card(Nine, suit) if isTrumpSuit(suit)               => 7
        case Card(Ace, suit) if isLeadingSuit(suit)              => 6
        case Card(King, suit) if isLeadingSuit(suit)             => 5
        case Card(Queen, suit) if isLeadingSuit(suit)            => 4
        case Card(Jack, suit) if isLeadingSuit(suit)             => 3
        case Card(Ten, suit) if isLeadingSuit(suit)              => 2
        case Card(Nine, suit) if isLeadingSuit(suit)             => 1
        case _                                                   => 0
      }

      val (winner, _) = state.alreadyPlayed.maxBy { case (_, card) =>
        value(card)
      }

      // Prepare state for next round
      val newState = state.copy(
        up = winner,
        firstPlayed = none,
        alreadyPlayed = Map.empty[Player, Card]
      )
      pure((newState, winner))

    }

    def playCard(implicit A: Applicative[F], PI: PlayerInteraction[F]) =
      StateT[F, RoundState, Card] { state: RoundState =>
        val hand = state.hands(state.up)
        val suit = (c: Card) =>
          c match {
            case Card(Jack, suit) if sameColor(suit) == state.trump =>
              state.trump
            case Card(_, suit) => suit
          }

        val leadingSuit = state.firstPlayed
        val onTable = state.alreadyPlayed

        val isLeftBower = Card.isLeftBower(state.trump, _)
        val removeFromHand = (c: Card) => hand.filterNot(_ === c)

        def choseOffSuit(card: Card) = {
          val rest = removeFromHand(card)
          val notSameSuitThatLead = !leadingSuit.contains(suit(card))
          lazy val restOfHandHasLeadSuit =
            leadingSuit.exists(s => rest.find(c => suit(c) == s).isDefined)

          (notSameSuitThatLead && restOfHandHasLeadSuit)
        }

        def getCard: F[Card] = for {
          card <- PI.playCard(
            state.up,
            hand,
            state.alreadyPlayed,
            state.trump,
            state.firstPlayed
          )
          checkCard <- if (choseOffSuit(card)) getCard else A.pure(card)
        } yield checkCard

        getCard.map { card =>
          (
            state.copy(
              hands = state.hands
                .updated(
                  state.up,
                  removeFromHand(card)
                ),
              firstPlayed = leadingSuit orElse suit(card).some,
              up = left(state.up),
              alreadyPlayed = onTable.updated(state.up, card)
            ),
            card
          )

        }

      }

    val play = for {
      w1 <- playRound
      w2 <- playRound
      w3 <- playRound
      w4 <- playRound
      w5 <- playRound
      wins = List(w1, w2, w3, w4, w5).map(Team.team)
      winner = wins.groupMapReduce(identity)(_ => 1)(_ + _).maxBy(_._2)
    } yield (winner._1, winner._2 === 5)

    for {
      trumpTeam <- determineTrump[F](dealer, flippedCard, hands)
      trump = trumpTeam.map(_._1)
      callers = trumpTeam.map(_._2)
      dealersHand <-
        if (trump.contains(flippedSuit))
          PI.askForDiscard(dealer, flippedCard, hands(dealer))
        else pure(hands(dealer))
      round <- trump.map { suit =>
        val newHands = hands.updated(dealer, dealersHand)
        val startState = RoundState(newHands, suit, none, left(dealer), Map.empty)
        play
          .run(startState)
          .map(_._2.some)
      } getOrElse pure(none)
    } yield round.map { case (team, all5) =>
      (team, all5, callers.contains(team))
    }

  }

  def determineTrump[F[_]](
      dealer: Player,
      flippedOver: Card,
      hands: Map[Player, Hand]
  )(implicit PI: PlayerInteraction[F], M: Monad[F]): F[Option[(Suit, Team)]] = {

    import Player._
    import PI._
    import M.pure

    val flipped = flippedOver.suit

    def withSuit(player: Player): F[Option[(Suit, Team)]] =
      for {
        answer <- askToCallTrumpFirstRound(player, hands(player), flippedOver)
        res <- answer match {
          case Pass if player === dealer => pure(none)
          case Pass                      => withSuit(left(player))
          case Pick(suit)                => pure((suit, Team.team(player)).some)
        }
      } yield res

    def withoutSuit(player: Player): F[Option[(Suit, Team)]] =
      for {
        answer <- askToCallTrumpSecondRound(player, hands(player), flippedOver)
        res <- answer match {
          case Pass if player === dealer      => pure(none)
          case Pass                           => withoutSuit(left(player))
          case Pick(suit) if suit === flipped => withoutSuit(player)
          case Pick(suit)                     => pure((suit, Team.team(player)).some)
        }
      } yield res

    val next = left(dealer)
    val firstRound = OptionT(withSuit(next))
    val secondRound = OptionT(withoutSuit(next))

    (firstRound orElse secondRound).value

  }

  def deal(deck: Deck): (List[Hand], Deck) = {
    val hand1 = deck.drop(0).take(2) ++ deck.drop(10).take(3)
    val hand2 = deck.drop(2).take(3) ++ deck.drop(13).take(2)
    val hand3 = deck.drop(5).take(2) ++ deck.drop(15).take(3)
    val hand4 = deck.drop(7).take(3) ++ deck.drop(18).take(2)
    val left = deck.drop(20)

    (List(hand1, hand2, hand3, hand4), left)
  }

  def determineFirstDealer[F[_]: Functor: Random]: F[Player] = {
    Random[F].nextInt.map(i =>
      i % 4 match {
        case 0 => North
        case 1 => South
        case 2 => East
        case 3 => West
      }
    )
  }

}

trait Random[F[_]] {
  def nextInt: F[Int]
}

object Random {
  def apply[F[_]](implicit R: Random[F]) = R
}
