package uk.co.danielrendall.adventofcode.y2015

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.Loc

object Day3 {

  val testData: LazyList[String] =
    """^v^v^v^v^v""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2015/day3.txt").lines

  def calculateLocs(directions: List[Char], visited: List[Loc]): List[Loc] =
    directions match
      case head :: rest =>
        head match
          case '^' => calculateLocs(rest, visited.head.up :: visited)
          case '>' => calculateLocs(rest, visited.head.right :: visited)
          case 'v' => calculateLocs(rest, visited.head.down :: visited)
          case '<' => calculateLocs(rest, visited.head.left :: visited)
      case _ => visited

  @main def d3p1(): Unit = {
    def solve(list: LazyList[String]) =
      calculateLocs(list.head.toList, List(Loc(0, 0)))
        .groupBy(identity)
        .count { case (loc, list) => list.nonEmpty }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d3p2(): Unit = {
    def solve(list: LazyList[String]) = {
      val allInstructions = list.head.toList.grouped(2).toList.unzip(c => (c(0), c(1)))

      val firstLocs = calculateLocs(allInstructions(0), List(Loc(0, 0)))
      val secondLocs = calculateLocs(allInstructions(1), List(Loc(0, 0)))
      (firstLocs ++ secondLocs)
        .groupBy(identity)
        .count { case (loc, list) => list.nonEmpty }
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }
}
