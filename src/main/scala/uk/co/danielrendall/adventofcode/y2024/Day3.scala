package uk.co.danielrendall.adventofcode.y2024

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day3 {

  val testData1: LazyList[String] =
    """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))""".stripMargin.splitAndTrimToList

  val testData2: LazyList[String] =
    """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2024/day3.txt").lines



  private val mulRegex = "mul\\(([0-9]+),([0-9]+)\\)".r

  private val dontRegex = "don't\\(\\).*?do\\(\\)".r

  @main def d3p1(): Unit = {

    def solve(list: LazyList[String]) =
      mulRegex.findAllMatchIn(list.mkString("")).map { mtch =>
        mtch.group(1).toInt * mtch.group(2).toInt
      }.sum

    println("Test: " + solve(testData1))
    println("Actual: " + solve(numbers))
  }

  @main def d3p2(): Unit = {

    def solve(list: LazyList[String]) =
      mulRegex.findAllMatchIn(dontRegex.replaceAllIn(list.mkString(""), "")).map { mtch =>
        mtch.group(1).toInt * mtch.group(2).toInt
      }.sum

    println("Test: " + solve(testData2))
    println("Actual: " + solve(numbers))
  }
}

