package uk.co.danielrendall.adventofcode

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day {

  val testData: LazyList[String] =
    """""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/202x/day.txt").lines

  @main def dxp1(): Unit = {
    def solve(list: LazyList[String]) = ""

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def dxp2(): Unit = {
    def solve(list: LazyList[String]) = ""

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }
}

