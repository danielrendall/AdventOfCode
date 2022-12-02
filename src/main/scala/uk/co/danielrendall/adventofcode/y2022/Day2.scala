package uk.co.danielrendall.adventofcode.y2022

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day2 {

  val testData: LazyList[String] =
    """A Y
      |B X
      |C Z""".stripMargin.splitAndTrimToList

  val games: LazyList[String] = this.getClass.getResourceAsStream("/2022/day2.txt").lines.filterNot(_.isEmpty)

  @main def d2p1(): Unit =
    def solve(list: LazyList[String]) = list.flatMap(round1Scores.get).sum

    println("Test: " +  solve(testData))
    println("Actual: " + solve(games))

  @main def d2p2(): Unit =
    def solve(list: LazyList[String]) = list.flatMap(round2Scores.get).sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(games))

  sealed abstract class Play(val beats: Char, val draws: Char, val score: Int) {
    def scoreFor(other: Char): Int =
      score + (if (other == beats) 6 else (if (other == draws) 3 else 0))

  }

  object Play {
    def apply(c: Char): Play = c match
      case 'A' => Rock
      case 'B' => Paper
      case 'C' => Scissors
      case 'X' => Rock
      case 'Y' => Paper
      case 'Z' => Scissors
      case _ => throw new IllegalArgumentException(s"$c")
  }

  case object Rock extends Play('C', 'A', 1)
  case object Paper extends Play( 'A', 'B',2)
  case object Scissors extends Play( 'B', 'C', 3)

  val allPlays: Seq[Play] = Seq(Rock, Paper, Scissors)

  private val round1Scores: Map[String, Int] =
    (for {
      opponent <- 'A' to 'C'
      us <- 'X' to 'Z'
    } yield {
      val ourObj = Play(us)
      s"$opponent $us" -> ourObj.scoreFor(opponent)
    }).toMap

  private val round2Scores: Map[String, Int] =
    (for {
      opponent <- 'A' to 'C'
    } yield {
      // X = lose, Y = draw, Z = win
      val losingPlay: Char = allPlays.find(p => p.beats != opponent && p.draws != opponent).get.draws
      val drawingPlay: Char = allPlays.find(p => p.draws == opponent).get.draws
      val winningPlay: Char = allPlays.find(p => p.beats == opponent).get.draws
      Seq(
        s"$opponent X" -> Play(losingPlay).scoreFor(opponent),
        s"$opponent Y" -> Play(drawingPlay).scoreFor(opponent),
        s"$opponent Z" -> Play(winningPlay).scoreFor(opponent)
      )

    }).flatten.toMap
}
