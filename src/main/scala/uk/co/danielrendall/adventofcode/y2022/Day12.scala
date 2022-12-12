
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.{Array2D, Loc}
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable

object Day12 {

  val testData: LazyList[String] =
    """Sabqponm
      |abcryxxl
      |accszExk
      |acctuvwj
      |abdefghi""".stripMargin.splitAndTrimToList

  val games: LazyList[String] = this.getClass.getResourceAsStream("/2022/day12.txt").lines.filterNot(_.isEmpty)

  @main def d12p1(): Unit =
    def solve(list: LazyList[String]) =
      val grid: Array2D[Char] = list.toBorderedArray(identity, ' ')

      val start = grid.findLocs(_ == 'S').headOption.getOrElse(Loc(1, 1))
      val end = grid.findLocs(_ == 'E').head

      val best =  computeShortestPath(grid, start, end)

      val path = plot(grid.width, grid.height, best.reverse)
      println(path.toString(""))
      best.length - 1

    println("Test: " + solve(testData))
    println("Actual: " + solve(games))

  @main def d12p2(): Unit =
    def solve(list: LazyList[String]) =
      val grid: Array2D[Char] = list.toBorderedArray(identity, ' ')

      val end = grid.findLocs(_ == 'E').head
      val paths = grid.findLocs(c => c == 'a' || c == 'S').map { start =>
        computeShortestPath(grid, start, end)
      }

      // Need to neglect paths of zero length meaning the end can't be reached
      val best = paths.filterNot(_.isEmpty).minBy(_.length)
      val path = plot(grid.width, grid.height, best.reverse)

      println(path.toString(""))
      best.length - 1

    println("Test: " + solve(testData))
    println("Actual: " + solve(games))


  /**
   * @param grid The grid of characters
   * @param bestPaths A grid of the best known paths to each point on the grid from the starting point
   * @param start Where to start the path
   * @param end The end point
   */
  def computeShortestPath(grid: Array2D[Char], start: Loc, end: Loc): List[Loc] = {
    val bestPaths: Array2D[List[Loc]] = Array2D.fill(grid.width, grid.height)(List.empty[Loc])

    @tailrec
    def check(toCheck: List[Loc]): Unit =
      toCheck match
        case head :: rest =>
          // We assume this is the best path to the current location
          val currentPath = bestPaths.get(head)
          assert(currentPath.nonEmpty, "Found empty path at " + head)
          val current: Char = grid.get(head)
          val possibleAdjacentLocs: Seq[Loc] = head
            .gridAdjacent
            .filter(grid.contains)
            .filter(move => canVisit(current, grid.get(move)))

          val adjacentLocationsToCheck: Seq[Loc] = possibleAdjacentLocs.flatMap { adjacent =>
            val pathToAdjacentFromHere = adjacent :: currentPath
            val bestCurrentPathToAdjacent = bestPaths.get(adjacent)
            // If we've never visited this adjacent thing, or its best current path is worse than the path from here
            // then we want to update it and queue it for inspection. We don't care about it if the current best path
            // is the same as the path to it from here
            if (bestCurrentPathToAdjacent.isEmpty || (bestCurrentPathToAdjacent.length > pathToAdjacentFromHere.length)) {
              bestPaths.set(adjacent, pathToAdjacentFromHere)
              Some(adjacent)
            } else {
              None
            }
          }
          check(rest.filterNot(_ == head) ++ adjacentLocationsToCheck)

        case _ => ()

    bestPaths.set(start, List(start))
    check(List(start))
    bestPaths.get(end)
  }

  def canVisit(current: Char, dest: Char): Boolean = {
    if (dest == 'S') false
    else if (current == 'S') dest == 'a'
    else if (dest == 'E') current == 'z'
    else (dest.toInt - current.toInt) <= 1
  }

  def plot(width: Int, height: Int, locs: List[Loc]): Array2D[Char] = {
    val grid: Array2D[Char] = Array2D.fill(width, height)('.')

    @tailrec
    def recurse(remaining: List[Loc], lastOpt: Option[Loc]): Unit =
      remaining match
        case head :: tail =>
          lastOpt match
            case Some(last) =>
              val symbol: Char =
                if (last.left == head) '<'
                else if (last.right == head) '>'
                else if (last.up == head) '^'
                else if (last.down == head) 'V'
                else throw new IllegalArgumentException("Bad direction")
              grid.set(last, symbol)
            case None =>
              grid.set(head, 'S')
          recurse(tail, Some(head))
        case _ =>
          lastOpt.foreach(g => grid.set(g, 'E'))
          ()

    recurse(locs, None)
    grid
  }

}
