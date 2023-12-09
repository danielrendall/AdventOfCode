package uk.co.danielrendall.adventofcode.y2023

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day9 {

  val testData: LazyList[String] =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2023/day9.txt").lines

  @main def d9p1(): Unit = {
    def solve(list: LazyList[String]) = {
      list.map(_.split(' ').map(_.trim.toLong).toList).toList
        .map(predictNextNumber)
        .sum
    }


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d9p2(): Unit = {

    def solve(list: LazyList[String]) = {
      list.map(_.split(' ').map(_.trim.toLong).toList.reverse).toList
        .map(predictNextNumber)
        .sum
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  def predictNextNumber(nums: List[Long]): Long =
    nums.headOption match {
      case Some(first) =>
        if (nums.tail.forall(_ == first)) {
          first
        } else {
          val diffs = nums.zip(nums.tail).map { case (first, second) => second - first}
          nums.last + predictNextNumber(diffs)
        }
      case None =>
        throw new IllegalArgumentException("Empty list")
    }
}

