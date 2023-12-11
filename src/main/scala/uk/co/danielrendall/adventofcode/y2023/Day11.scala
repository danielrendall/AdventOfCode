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
      val expanded = expand(grid)
      val locs = expanded.findLocs(_ == '#').toList
      @tailrec
      def findAllDistances(remaining: List[Loc], count: Int): Int =
        remaining match {
          case head :: rest =>
            val newCount = rest.foldLeft(count) { case (count, otherLoc) =>
              val yDiff = Math.abs(otherLoc.y - head.y)
              val xDiff = Math.abs(otherLoc.x - head.x)
              count + xDiff + yDiff
            }
            findAllDistances(rest, newCount)

          case _ =>
            count
        }
      findAllDistances(locs, 0)
    }


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d11p2(): Unit = {

    def solve(list: LazyList[String]) = ""

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  private def expand(grid: Array2D[Char]): Array2D[Char] = {
    val blankRows = grid.rowIteratorLR
      .zipWithIndex
      .filter { case (rowIt, _) => rowIt.forall(l => grid.get(l) == '.')}
      .map { case (_, idx) => idx + 1 }
      .toList
      .reverse

    val blankCols = grid.columnIteratorTD
      .zipWithIndex
      .filter { case (colIt, _) => colIt.forall(l => grid.get(l) == '.') }
      .map { case (_, idx) => idx + 1 }
      .toList
      .reverse
    val expandedWithRows = blankRows.foldLeft(grid) { case (grid, idx) => grid.insertRow(idx, '.')}
    blankCols.foldLeft(expandedWithRows) { case (grid, idx) => grid.insertCol(idx, '.')}
  }
}

