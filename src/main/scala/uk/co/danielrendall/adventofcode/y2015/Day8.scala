package uk.co.danielrendall.adventofcode.y2015

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day8 {

  val testData: LazyList[String] =
    """""
      |"abc"
      |"aaa\"aaa"
      |"\x27"""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2015/day8.txt").lines

  @tailrec
  def computeCharsInMemory(string: List[Char],
                           accum: Int,
                           lastCharWasBackslash: Boolean,
                           escapedCharsToSkip: Int): Int =
    string match
      case head :: rest =>
        if (escapedCharsToSkip > 0) {
          if (lastCharWasBackslash && head == 'x') {
            // escape sequence
            computeCharsInMemory(rest, accum, false, 2)
          } else {
            computeCharsInMemory(rest, accum, false, escapedCharsToSkip - 1)
          }
        } else {
          if (head == '\\') {
            computeCharsInMemory(rest, accum + 1, true, 1)
          } else {
            computeCharsInMemory(rest, accum + 1, false, 0)
          }
        }

      case _ =>
        accum - 2

  @main def d8p1(): Unit = {
    def solve(list: LazyList[String]) =
      list.map(s => s.length - computeCharsInMemory(s.toList, 0, false, 0)).sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d8p2(): Unit = {
    def solve(list: LazyList[String]) =
      list.map(s => s.count(c => c == '"' || c == '\\') + 2).sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }
}

