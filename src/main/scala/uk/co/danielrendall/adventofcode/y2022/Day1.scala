package uk.co.danielrendall.adventofcode.y2022

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day1 {

  /**
   * Numbers represent calories.
   *
   * Part 1
   * Sum groups of calories.
   *
   * Part 2
   * Take the top three groups and sum them.
   */

  val testData: LazyList[String] =
    """1000
      |2000
      |3000
      |
      |4000
      |
      |5000
      |6000
      |
      |7000
      |8000
      |9000
      |
      |10000""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2022/day1.txt").lines

  @main def d1p1(): Unit =
    def solve(list: LazyList[String]) =
      list.groupSeparatedBy("").map(_.map(_.toInt).sum).max

    println("Test: " +  solve(testData))
    println("Actual: " + solve(numbers))

  @main def d1p2(): Unit =
    def solve(list: LazyList[String]) =
      list.groupSeparatedBy("").map(_.map(_.toInt).sum).sorted(Ordering.Int.reverse).take(3).sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
}
