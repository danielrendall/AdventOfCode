package uk.co.danielrendall.adventofcode.y2023

import uk.co.danielrendall.adventofcode.utils.ArrayUtils
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.Array2D
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day13 {

  val testData: LazyList[String] =
    """#.##..##.
      |..#.##.#.
      |##......#
      |##......#
      |..#.##.#.
      |..##..##.
      |#.#.##.#.
      |
      |#...##..#
      |#....#..#
      |..##..###
      |#####.##.
      |#####.##.
      |..##..###
      |#....#..#""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2023/day13.txt").lines

  @main def d13p1(): Unit = {
    // Not 39240
    def solve(list: LazyList[String]) = {
      val grids = parse(list)
      grids.map(solvePart1).sum
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d13p2(): Unit = {

    def solve(list: LazyList[String]) = ""

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  private def parse(lines: LazyList[String]): LazyList[Array2D[Char]] =
    if (lines.nonEmpty) {
      val group = lines.takeWhile(_.trim.nonEmpty)
      val array = ArrayUtils.buildBorderedArray[Char](group, identity, '.')
      array #:: parse(lines.drop(group.length + 1))
    } else LazyList.empty

  private def solvePart1(grid: Array2D[Char]): Int = {
    println(grid.toString(""))
    val h = horizontalLinesOfReflection(grid)
    val v = verticalLinesOfReflection(grid)
    println("h = " + h.mkString(", "))
    println("v = " + v.mkString(", "))
    h.sum + 100 * v.sum
  }

  private def verticalLinesOfReflection(grid: Array2D[Char]): Seq[Int] = {
    val possibles = grid.rowIteratorLR.map { it =>
      val chars = it.map(grid.get).toList
      findMirrorPoints(chars)
    }
    possibles.head.filter(v => possibles.forall(_.contains(v)))
  }

  private def horizontalLinesOfReflection(grid: Array2D[Char]): Seq[Int] = {
    val possibles = grid.columnIteratorTD.map { it =>
      val chars = it.map(grid.get).toList
      findMirrorPoints(chars)
    }
    possibles.head.filter(v => possibles.forall(_.contains(v)))
  }

  private def findMirrorPoints(chars: List[Char]): Seq[Int] = {
    (1 until (chars.length - 1)).flatMap { split =>
      val backwards = (0 until split).map(i => chars(split - i))
      val forwards = ((split + 1) until chars.length).map(i => chars(i))
      if ((backwards.zip(forwards)).forall { case (b, f) => b == f }) Some(split + 1) else None
    }
  }


}

