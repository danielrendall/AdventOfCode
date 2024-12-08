package uk.co.danielrendall.adventofcode.y2024

import uk.co.danielrendall.adventofcode.utils.ArrayUtils
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.{Array2D, Loc}
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day8 {

  val testData: LazyList[String] =
    """............
      |........0...
      |.....0......
      |.......0....
      |....0.......
      |......A.....
      |............
      |............
      |........A...
      |.........A..
      |............
      |............""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2024/day8.txt").lines

  @tailrec
  def buildAntennaeTypeList(list: List[(Loc, Char)], accum: Map[Char, Set[Loc]]): Map[Char, Set[Loc]] =
    list match
      case head :: rest =>
        if (head._2 != '.') {
          accum.get(head._2) match
            case Some(set) =>
              buildAntennaeTypeList(rest, accum.updated(head._2, set + head._1))
            case None =>
              buildAntennaeTypeList(rest, accum.updated(head._2, Set(head._1)))
        } else {
          buildAntennaeTypeList(rest, accum)
        }
      case _ =>
        accum

  def getAntinodesPart1(set: Set[Loc]): Set[Loc] =
    for {
      loc1 <- set
      loc2 <- set
      if loc1 != loc2
    } yield {
      val xDisp = loc2.x - loc1.x
      val yDisp = loc2.y - loc1.y
      loc2.right(xDisp).down(yDisp)
    }

  def getAntinodesPart2(grid: Array2D[Char], set: Set[Loc]) =
    for {
      loc1 <- set
      loc2 <- set
      if loc1 != loc2
    } yield {
      val xDisp = loc2.x - loc1.x
      val yDisp = loc2.y - loc1.y
      (Iterable.unfold(loc1) { curLoc =>
        val newLoc = curLoc.right(xDisp).down(yDisp)
        if (grid.contains(newLoc)) {
          Some((newLoc, newLoc))
        } else {
          None
        }
      }).toSet
    }

  @main def d8p1(): Unit = {
    def solve(list: LazyList[String]) =
      val grid: ArrayUtils.Array2D[Char] = ArrayUtils.buildBorderedArray(list, identity, ' ')
      val map = buildAntennaeTypeList(grid.locsAndValues.toList, Map.empty)
      val antinodeLocs: Iterable[Loc] = map.values.map(getAntinodesPart1).flatMap(_.filter(grid.contains)).toSet
      antinodeLocs.size

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d8p2(): Unit = {
    def solve(list: LazyList[String]) =
      val grid: Array2D[Char] = ArrayUtils.buildBorderedArray(list, identity, ' ')
      val map = buildAntennaeTypeList(grid.locsAndValues.toList, Map.empty)
      val antinodeLocs: Iterable[Loc] = map.values.flatMap(locs => getAntinodesPart2(grid, locs)).flatten.toSet
      antinodeLocs.size

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }
}

