package uk.co.danielrendall.adventofcode.y2023

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day1 {

  val testData1: LazyList[String] =
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin.splitAndTrimToList

  val testData2: LazyList[String] =
    """two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2023/day1.txt").lines

  @main def d1p1(): Unit = {
    def solve(list: LazyList[String]) =
      list.map(s => {
        val nums = s.filter(_.isDigit)
        s"${nums.head}${nums.last}".toInt
      }).sum

    println("Test: " + solve(testData1))
    println("Actual: " + solve(numbers))
  }

  val nums: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  @main def d1p2(): Unit = {

    val forwardRegex = s"${nums.mkString("|")}|[0-9]".r
    val forwardMap = nums.zipWithIndex.toMap
    val reversedNums = nums.map(_.reverse)
    val reverseRegex = s"${reversedNums.mkString("|")}|[0-9]".r
    val reverseMap = reversedNums.zipWithIndex.toMap

    def solve(list: LazyList[String]) =
      list.flatMap(s => {
        for {
          first <- forwardRegex.findFirstMatchIn(s).map(_.matched)
          last <- reverseRegex.findFirstMatchIn(s.reverse).map(_.matched)
        } yield {
          val firstDigit = forwardMap.getOrElse(first, s"${first.head}".toInt)
          val lastDigit = reverseMap.getOrElse(last, s"${last.head}".toInt)
          s"${firstDigit}${lastDigit}".toInt
        }
      }).sum

    println("Test: " + solve(testData2))
    println("Actual: " + solve(numbers))
  }
}
