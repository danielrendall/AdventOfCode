package uk.co.danielrendall.adventofcode.y2022

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.util.matching.Regex

object Day4 {

  val RangePair: Regex = "([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)".r("g1", "g2", "g3", "g4")

  val testData: LazyList[Pair[Range]] =
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8""".stripMargin.splitAndTrimToList.map {
      case RangePair(g1, g2, g3, g4) => Pair(Range(g1, g2), Range(g3, g4))
    }

  val games: LazyList[Pair[Range]] = this.getClass.getResourceAsStream("/2022/day4.txt").lines.filterNot(_.isEmpty).map {
    case RangePair(g1, g2, g3, g4) => Pair(Range(g1, g2), Range(g3, g4))
  }

  @main def d4p1(): Unit =
    def solve(list: LazyList[Pair[Range]]) =
      list.count(r => r.first.fullyContains(r.second) || r.second.fullyContains(r.first))

    println("Test: " + solve(testData))
    println("Actual: " + solve(games))

  @main def d4p2(): Unit =
    def solve(list: LazyList[Pair[Range]]) =
      list.count(r => !(r.first.end < r.second.start || r.first.start > r.second.end))

    println("Test: " + solve(testData))
    println("Actual: " + solve(games))

  case class Range(start: Int, end: Int) {
    // Inclusive
    assert (start <= end)

    def fullyContains(other: Range): Boolean = start <= other.start && end >= other.end
  }

  object Range {
    def apply(s1: String, s2: String): Range = Range(s1.toInt, s2.toInt)
  }

  case class Pair[T](first: T, second: T)
}
