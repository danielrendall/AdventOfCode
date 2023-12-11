package uk.co.danielrendall.adventofcode.y2023

import uk.co.danielrendall.adventofcode.utils.ArrayUtils
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.{Array2D, Loc}
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day11 {

  val testData: LazyList[String] =
    """...#......
      |.......#..
      |#.........
      |..........
      |......#...
      |.#........
      |.........#
      |..........
      |.......#..
      |#...#.....""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2023/day11.txt").lines

  @main def d11p1(): Unit = {
    def solve(list: LazyList[String]) = {
      val grid = ArrayUtils.buildBorderedArray(list, identity, '.')
      solveWithExpansionFactor(grid, 2)
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d11p2(): Unit = {

    def solve(list: LazyList[String]) = {
      val grid = ArrayUtils.buildBorderedArray(list, identity, '.')
      solveWithExpansionFactor(grid, 1000000)
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  private def solveWithExpansionFactor(grid: Array2D[Char],
                                       expansionFactor: Int) = {
    val locs = grid.findLocs(_ == '#').toList
    val blankRowIndexes: List[Int] = grid.rowIteratorLR
      .zipWithIndex
      .filter { case (rowIt, _) => rowIt.forall(l => grid.get(l) == '.') }
      .map { case (_, idx) => idx + 1 }
      .toList
    val blankColumnIndexes: List[Int] = grid.columnIteratorTD
      .zipWithIndex
      .filter { case (colIt, _) => colIt.forall(l => grid.get(l) == '.') }
      .map { case (_, idx) => idx + 1 }
      .toList

    @tailrec
    def findAllDistances(remaining: List[Loc], count: Long): Long =
      remaining match {
        case head :: rest =>
          val newCount = rest.foldLeft(count) { case (count, otherLoc) =>
            val xMax = Math.max(otherLoc.x, head.x)
            val xMin = Math.min(otherLoc.x, head.x)
            val yMax = Math.max(otherLoc.y, head.y)
            val yMin = Math.min(otherLoc.y, head.y)
            val xAdditions = blankColumnIndexes.count(idx => idx > xMin && idx < xMax) * (expansionFactor - 1)
            val yAdditions = blankRowIndexes.count(idx => idx > yMin && idx < yMax) * (expansionFactor - 1)
            count + (xMax - xMin + xAdditions) + (yMax - yMin + yAdditions)
          }
          findAllDistances(rest, newCount)

        case _ =>
          count
      }

    findAllDistances(locs, 0L)

  }
}

