package uk.co.danielrendall.adventofcode.y2023

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day4 {

  val testData: LazyList[String] =
    """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
      |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
      |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
      |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
      |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
      |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2023/day4.txt").lines

  val scores: Array[Int] = Array(0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)

  @main def d4p1(): Unit = {
    def solve(list: LazyList[String]) = list.map { s =>
      scores(getNumberOfWinningNumbers(s))
    }.sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d4p2(): Unit = {

    def solve(list: LazyList[String]): Int = {

      @tailrec
      def process(remaining: List[String], counts: List[Int], totalCards: Int): Int =
        remaining match {
          case head :: rest =>
            val copiesOfThisCard = counts.head
            val numberOfWinningNumbers = getNumberOfWinningNumbers(head)
            val remainingCounts = counts.tail
            val newCounts = remainingCounts.take(numberOfWinningNumbers).map(_ + copiesOfThisCard).concat(remainingCounts.drop(numberOfWinningNumbers))
            process(rest, newCounts, totalCards + copiesOfThisCard)
          case _ =>
            totalCards
        }

      val all = list.toList
      val count = all.size
      val initCounts = List.fill(count)(1)
      process(all, initCounts, 0)
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  def getNumberOfWinningNumbers(card: String): Int = {
    val b1 = card.split(':').map(_.trim)
    val b2 = b1(1).split('|').map(_.trim)
    val winning = b2(0).split(' ').map(_.trim).filterNot(_.isEmpty).map(_.toInt).toSet
    val nums = b2(1).split(' ').map(_.trim).filterNot(_.isEmpty).map(_.toInt).toSet
    nums.count(winning.contains)
  }
}
