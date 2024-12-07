package uk.co.danielrendall.adventofcode.y2015

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day1 {

  val testData: LazyList[String] =
    """()())""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2015/day1.txt").lines

  @tailrec
  def getFloor(remaining: List[Char], floor: Int): Int =
    remaining match
      case head :: rest => if (head == '(') then getFloor(rest, floor + 1) else getFloor(rest, floor - 1)
      case _ => floor

  @tailrec
  def getBasementPos(remaining: List[Char], floor: Int, count: Int): Int =
    if (floor < 0) then count else {
    remaining match
      case head :: rest => if (head == '(') then getBasementPos(rest, floor + 1, count + 1) else getBasementPos(rest, floor - 1, count + 1)
      case _ => throw new IllegalArgumentException("Ran out of characters")
      }


  @main def d1p1(): Unit = {
    def solve(list: LazyList[String]) =
      getFloor(list.head.toList, 0)


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d1p2(): Unit = {
    def solve(list: LazyList[String]) =
      getBasementPos(list.head.toList, 0, 0)

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }
}


