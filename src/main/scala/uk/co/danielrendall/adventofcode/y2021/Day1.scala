package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day1 {

  /**
   * Numbers represent depths.
   *
   * Part 1
   * Count how many times a number is greater than the number preceding it.
   *
   * Part 2
   * Group the data into (overlapping) windows of size 3; calculate the sum in each window, and count the number of
   * times the sum for a window is greater than that of the preceding window
   * 
   */

  val testData: LazyList[Int] =
    """199
      |200
      |208
      |210
      |200
      |207
      |240
      |269
      |260
      |263""".stripMargin.splitAndTrimToList.map(_.toInt)

  val numbers: LazyList[Int] = this.getClass.getResourceAsStream("/2021/day1.txt").lines.filterNot(_.isEmpty).map(_.toInt)

  @main def d1p1(): Unit =
    def solve(list: LazyList[Int]) = list.zip(list.tail).count { case (f, s) => f < s }

    println("Test: " +  solve(testData))
    println("Actual: " + solve(numbers))

  @main def d1p2(): Unit =
    val windowSize = 3
    def solve(list: LazyList[Int]) = {
      val windowedList: LazyList[Int] = list.windowed(3).map(_.sum)
      windowedList.zip(windowedList.tail).count { case (f, s) => f < s }
    }

    println("Test: " +  solve(testData))
    println("Actual: " + solve(numbers))
}
