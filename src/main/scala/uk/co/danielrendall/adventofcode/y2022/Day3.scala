package uk.co.danielrendall.adventofcode.y2022

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day3 {

  val testData: LazyList[String] =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin.splitAndTrimToList

  val games: LazyList[String] = this.getClass.getResourceAsStream("/2022/day3.txt").lines.filterNot(_.isEmpty)

  @main def d3p1(): Unit =
    def solve(list: LazyList[String]) =
      list.map { s =>
        val ints = s.map(valueOf)
        val halfWay = ints.length / 2
        val firstHalf = ints.slice(0, halfWay).toSet
        val secondHalf = ints.slice(halfWay / 2, ints.length).toSet
        (firstHalf intersect secondHalf).head
      }.sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(games))

  @main def d3p2(): Unit =
    def solve(list: LazyList[String]) =
      list.grouped(3).map { group =>
        group.map(_.toSet).reduce(_ intersect _).map(valueOf).head
      }.sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(games))

  def valueOf(c: Char): Int = c match
    case x if x.isLower => x.toInt - 96
    case x if x.isUpper => x.toInt - 38
    case _ => throw new IllegalArgumentException("Bad char " + c)
}
