package uk.co.danielrendall.adventofcode.y2024

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.util.matching.Regex

object Day1 {

  val testData: LazyList[String] =
    """3   4
      |4   3
      |2   5
      |1   3
      |3   9
      |3   3""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2024/day1.txt").lines

  private val NumberRegex: Regex = "^([0-9]+)\\s+([0-9]+)$".r

  @main def d1p1(): Unit = {
    def solve(list: LazyList[String]) =
      val pairs: (LazyList[Int], LazyList[Int]) = list.collect {
        case NumberRegex(num1, num2) => (num1.toInt, num2.toInt)
      }.unzip[Int, Int]
      (pairs._1.sorted zip pairs._2.sorted).map { case (a, b) => Math.abs(a - b)}.sum


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d1p2(): Unit = {

    def solve(list: LazyList[String]) =
      val pairs: (LazyList[Int], LazyList[Int]) = list.collect {
        case NumberRegex(num1, num2) => (num1.toInt, num2.toInt)
      }.unzip[Int, Int]
      val firstList = pairs._1
      val secondList = pairs._2
      val countsInSecondList: Map[Int, Int] = secondList.groupBy(identity).map { case (num, list) => (num, list.length)}
      firstList.groupBy(identity).map { case (num, list) => num * list.length * countsInSecondList.getOrElse(num, 0)}.sum


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }
}

