package uk.co.danielrendall.adventofcode.y2024

import uk.co.danielrendall.adventofcode.utils.ArrayUtils
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.LinearDirection.*
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.{Array2D, LinearDirection, Loc}
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day12 {

  val testData: LazyList[String] =
    """RRRRIICCFF
      |RRRRIICCCF
      |VVRRRCCFFF
      |VVRCCCJFFF
      |VVVVCJJCFE
      |VVIVCCJJEE
      |VVIIICJJEE
      |MIIIIIJJEE
      |MIIISIJEEE
      |MMMISSJEEE""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2024/day12.txt").lines

  def findRegions(array: Array2D[Char],
                  toSearch: Vector[Loc],
                  currentRegion: Region,
                  currentSearched: Set[Loc],
                  currentPerimeter: Int,
                  currentEdgeMap: Map[LinearDirection, Set[Loc]],
                  regions: List[Region]): List[Region] = {
    lazy val marker = currentRegion.letter.toLower
    toSearch.headOption match
      case Some(nextLocInRegion) =>
        if (!currentSearched.contains(nextLocInRegion)) {
          val (perimeterAdjustment: Int,
          adjacentToSearch: List[Loc],
          newEdgeMap: Map[LinearDirection, Set[Loc]]) =
            LinearDirection.all.foldLeft((0, List.empty[Loc], currentEdgeMap)) { case ((p, locs, map), dir) =>
              val x = 0
              val nextLoc = dir(nextLocInRegion)
              val c = array.get(nextLoc)
              if (c == currentRegion.letter) {
                // In this region, unsearched
                (p, nextLoc :: locs, map)
              } else if (c == marker) {
                // In this region, searched
                (p, locs, map)
              } else {
                // Outside this region; there's an edge
                (p + 1, locs, map.updated(dir, map(dir) + nextLoc))
              }
            }
          array.set(nextLocInRegion, marker)
          findRegions(array,
            toSearch.tail ++ adjacentToSearch,
            currentRegion,
            currentSearched + nextLocInRegion,
            currentPerimeter + perimeterAdjustment,
            newEdgeMap,
            regions)
        } else {
          findRegions(array,
            toSearch.tail,
            currentRegion,
            currentSearched + nextLocInRegion,
            currentPerimeter,
            currentEdgeMap,
            regions)
        }
      // Do thing
      case None =>
        val finishedRegion = currentRegion.copy(contains = currentSearched.toSet, perimeter = currentPerimeter, edgeCount = computeEdgeCount(currentEdgeMap))
        finishedRegion.contains.foreach(l => array.set(l, '.'))
        array.findLocsLazy(c => c != ' ' && c != '.').headOption match
          case Some(newLoc) =>
            val newRegion = Region(finishedRegion.number + 1, array.get(newLoc), Set.empty, 0, 0)
            findRegions(array, Vector(newLoc), newRegion, Set.empty, 0, emptyEdgeLocMap(), finishedRegion :: regions)
          case None =>
            // Take the tail because we get a useless empty region at the start
            (finishedRegion :: regions).reverse.tail
  }

  def emptyEdgeLocMap(): Map[LinearDirection, Set[Loc]] = Map(
    Left -> Set.empty[Loc],
    Up -> Set.empty[Loc],
    Right -> Set.empty[Loc],
    Down -> Set.empty[Loc]
  )

  def computeEdgeCount(edgeLocMap: Map[LinearDirection, Set[Loc]]): Int =
    countEdges(edgeLocMap(Left), _.x, _.y) +
    countEdges(edgeLocMap(Up), _.y, _.x) +
    countEdges(edgeLocMap(Right), _.x, _.y) +
    countEdges(edgeLocMap(Down), _.y, _.x)

  def countEdges(locs: Set[Loc], groupByFn: Loc => Int, sortByFn: Loc => Int): Int = {
    locs.groupBy(groupByFn).map { case (_, values) => values.toSeq.sortBy(sortByFn)}.map(seq => countContiguous(seq, sortByFn)).sum
  }

  def countContiguous(locs: Seq[Loc], posFn: Loc => Int): Int = {
    @tailrec
    def count(remaining: List[Loc], last: Int, edgeCount: Int): Int =
      remaining match
        case head :: rest =>
          val pos = posFn(head)
          if (pos == last + 1) {
            count(rest, pos, edgeCount)
          } else {
            count(rest, pos, edgeCount + 1)
          }
        case _ =>
          edgeCount

    count(locs.toList, Int.MinValue, 0)
  }

  @main def d12p1(): Unit = {
    def solve(list: LazyList[String]) =
      val array = ArrayUtils.buildBorderedArray(list, identity, ' ')
      val regions = findRegions(array, Vector.empty, Region.empty, Set.empty, 0, emptyEdgeLocMap(), List.empty)
      regions.map(_.pricePart1).sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d12p2(): Unit = {
    def solve(list: LazyList[String]) =
      val array = ArrayUtils.buildBorderedArray(list, identity, ' ')
      val regions = findRegions(array, Vector.empty, Region.empty, Set.empty, 0, emptyEdgeLocMap(), List.empty)
      regions.map(_.pricePart2).sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  case class Region(number: Int,
                    letter: Char,
                    contains: Set[Loc],
                    perimeter: Int,
                    edgeCount: Int) {
    def size: Int = contains.size

    def pricePart1 = perimeter * size

    def pricePart2 = edgeCount * size
  }

  object Region {
    val empty: Region = Region(-1, ' ', Set.empty, 0, 0)
  }
}
