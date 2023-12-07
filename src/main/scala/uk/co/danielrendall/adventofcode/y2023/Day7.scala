package uk.co.danielrendall.adventofcode.y2023

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*
import uk.co.danielrendall.adventofcode.y2023.Day7.Hand.{FIVE_OF_A_KIND, FOUR_OF_A_KIND, FULL_HOUSE, HIGH_CARD, ONE_PAIR, THREE_OF_A_KIND, TWO_PAIRS}

import scala.collection.mutable

object Day7 {

  val testData: LazyList[String] =
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2023/day7.txt").lines

  @main def d7p1(): Unit = {
    def solve(list: LazyList[String]) = {
      parse(list).sorted(Hand.orderingPart1).zipWithIndex.map { case (card, idx) => card.score * (idx + 1)}.sum
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d7p2(): Unit = {

    def solve(list: LazyList[String]) = {
      parse(list).sorted(Hand.orderingPart2).zipWithIndex.map { case (card, idx) => card.score * (idx + 1)}.sum
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  private def parse(list: Seq[String]): List[Hand] = list.map { s =>
    s.split(' ').toList match {
      case a :: b :: Nil => Hand(a, b.toInt)
      case _ => throw new Exception("Bad string: " + s)
    }
  }.toList

  case class Hand(cards: String, score: Int) {

    lazy val part1ScoredCards: Seq[Int] = cards.map {
      case 'A' => 0
      case 'K' => 1
      case 'Q' => 2
      case 'J' => 3
      case 'T' => 4
      case '9' => 5
      case '8' => 6
      case '7' => 7
      case '6' => 8
      case '5' => 9
      case '4' => 10
      case '3' => 11
      case '2' => 12
    }

    lazy val part2ScoredCards: Seq[Int] = cards.map {
      case 'A' => 0
      case 'K' => 1
      case 'Q' => 2
      case 'T' => 3
      case '9' => 4
      case '8' => 5
      case '7' => 6
      case '6' => 7
      case '5' => 8
      case '4' => 9
      case '3' => 10
      case '2' => 11
      case 'J' => 12
    }

    lazy val part1HandType: Int = {
      val cardsAndCounts = getCardsAndCounts
      cardsAndCounts.size match {
        case 1 => FIVE_OF_A_KIND
        case 2 =>
          if (cardsAndCounts.exists { case (c, i) => i == 4}) {
            FOUR_OF_A_KIND
          } else {
            FULL_HOUSE
          }
        case 3 =>
          if (cardsAndCounts.exists { case (c, i) => i == 3}) {
            THREE_OF_A_KIND
          } else {
            TWO_PAIRS
          }
        case 4 =>
          ONE_PAIR
        case _ =>
          HIGH_CARD
      }
    }

    lazy val part2HandType: Int = {
      val cardsAndCounts = getCardsAndCounts
      cardsAndCounts.size match {
        case 1 =>
          FIVE_OF_A_KIND
        case 2 =>
          cardsAndCounts.getOrElse('J', 0) match {
            case 0 =>
              if (cardsAndCounts.exists { case (c, i) => i == 4}) {
                FOUR_OF_A_KIND
              } else {
                FULL_HOUSE
              }
            case _ => // If one of the cards is a joker, we can make it pretend to be the other card to get 5 of a kind
              FIVE_OF_A_KIND
          }
        case 3 =>
          cardsAndCounts.getOrElse('J', 0) match {
            case 0 =>
              if (cardsAndCounts.exists { case (c, i) => i == 3}) {
                THREE_OF_A_KIND
              } else {
                TWO_PAIRS
              }
            case 1 =>
              if (cardsAndCounts.exists { case (c, i) => i == 3 }) {
                // There are 3 different cards, one of which appears 3 times so the other two must be different, and
                // one of these is the joker. If we made the joker into the card that appears 3 times there would be
                // four of a kind
                FOUR_OF_A_KIND
              } else {
                // There are 3 different cards; none appears 3 times, so we have two pairs, and the lone card is the
                // joker. This can be made to match one of the pairs, giving us a full house
                FULL_HOUSE
              }
            case 2 =>
              // There are 3 different cards, the joker appears exactly twice, therefore it must be one of the pairs
              // in a two pairs hand. We can make it match the other pair, giving us 4 of a kind
              FOUR_OF_A_KIND
            case 3 =>
              // There are 3 different cards, the joker appears 3 times. We can turn it into one of the other two cards
              // giving us four of a kind
              FOUR_OF_A_KIND
          }
        case 4 =>
          cardsAndCounts.getOrElse('J', 0) match {
            case 0 =>
              ONE_PAIR
            case _ =>
              // Either joker is an odd card out; we can make it match the pair for 3 of a kind, or the pair is a pair
              // of jokers which can be made to match another card for 3 of a kind
              THREE_OF_A_KIND
          }
        case _ =>
          cardsAndCounts.getOrElse('J', 0) match {
            case 0 =>
              HIGH_CARD
            case _ =>
              // There's a joker, so we can make a pair
              ONE_PAIR
          }
      }
    }

    private def getCardsAndCounts = cards.foldLeft(new mutable.HashMap[Char, Int]()) { (map, char) =>
      map.put(char, map.getOrElse(char, 0) + 1); map
    }
  }

  object Hand {

    val FIVE_OF_A_KIND = 0
    val FOUR_OF_A_KIND = 1
    val FULL_HOUSE = 2
    val THREE_OF_A_KIND = 3
    val TWO_PAIRS = 4
    val ONE_PAIR = 5
    val HIGH_CARD = 6

    implicit val orderingPart1: Ordering[Hand] = (x: Hand, y: Hand) => {
      val byType = y.part1HandType - x.part1HandType
      if (byType != 0) {
        byType
      } else {
        y.part1ScoredCards.zip(x.part1ScoredCards).map { case (yCard, xCard) => yCard - xCard }.find(_ != 0).getOrElse(0)
      }
    }

    implicit val orderingPart2: Ordering[Hand] = (x: Hand, y: Hand) => {
      val byType = y.part2HandType - x.part2HandType
      if (byType != 0) {
        byType
      } else {
        y.part2ScoredCards.zip(x.part2ScoredCards).map { case (yCard, xCard) => yCard - xCard }.find(_ != 0).getOrElse(0)
      }
    }

  }
}

