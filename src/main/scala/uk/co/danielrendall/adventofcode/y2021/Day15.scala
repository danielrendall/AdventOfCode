package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.ArrayUtils
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.*
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*
import uk.co.danielrendall.adventofcode.y2021.Day15.increment

import scala.annotation.tailrec
import scala.collection.immutable.Queue
object Day15 {

  val testData: LazyList[String] =
    """1163751742
      |1381373672
      |2136511328
      |3694931569
      |7463417111
      |1319128137
      |1359912421
      |3125421639
      |1293138521
      |2311944581""".stripMargin.splitAndTrimToList

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day15.txt").lines.filterNot(_.isEmpty)


  @main def d15p1() =
    def solve(seq: LazyList[String]): Unit =
      val bottomRight = solveForData(seq)
      println("Path length: " + bottomRight.bestPathSoFar.size + " risk: " + bottomRight.risk)

    solve(testData)
    solve(data)

  @main def d15p2() =
    def solve(seq: LazyList[String]): Unit =
      val repeatedX: List[String] = seq.map { (string: String) =>
        (string #:: LazyList.unfold(string){ s =>
          val next = s.map(increment)
          Some((next, next))
        }.take(4)).mkString
      }.toList

      val repeatedY: List[List[String]] =
        (repeatedX #:: LazyList.unfold(repeatedX){ l =>
          val next = l.map(_.map(increment))
          Some((next, next))
        }.take(4)).toList

      val finaldata: Seq[String] = repeatedY.flatten

      val bottomRight = solveForData(finaldata)
      println("Path length: " + bottomRight.bestPathSoFar.size + " risk: " + bottomRight.risk)




    solve(testData)
    solve(data)

  def increment(c: Char): Char = if (c == '9') '1' else (c.toInt + 1).toChar


  def solveForData(seq: Seq[String]): RiskAndPath =
    val array2d: Array2D[RiskAndPath] =
      buildBorderedArray[RiskAndPath](seq, c => RiskAndPath(s"$c".toInt), RiskAndPath(Integer.MAX_VALUE))

    solveForArray(array2d)


  def solveForArray(array2d: Array2D[RiskAndPath]): RiskAndPath =
    val initialState = State(array2d.update(Loc(1, 1), _.copy(risk = 0, bestPathSoFar = List(Loc(1, 1)))), Queue(Loc(1, 1)))

    val states: LazyList[State] = LazyList.unfold(initialState) { currentState =>
      currentState.locationsToCheck.dequeueOption.map { case (loc, restOfQueue) =>
        val currentRiskAndPath: RiskAndPath = currentState.array2d.get(loc)

        val (updatedArray, newLocationsToConsider) = loc.gridAdjacent.filter(array2d.contains).foldLeft((currentState.array2d, List.empty[Loc])) { case ((array, newLocations), adjacentLoc) =>
          // can we get to the adjacent loc more cheaply than current
          val currentBestPathToAdjacentLoc = array.get(adjacentLoc)
          // The risk for getting to the adjacent loc would be the risk for the current plus the adjacent's value; if
          // this is less than the current risk for the adjacent loc then we've found a better path.
          val possiblyBetterRisk = currentRiskAndPath.risk + currentBestPathToAdjacentLoc.value

          if (possiblyBetterRisk < currentBestPathToAdjacentLoc.risk) {
            // A better path; need to update the array, and add the new location for investigation
            // We copy it to preserve its value
            // It is _much_ faster to run if we mutate the array rather than making a copy...
            (array.updateMut(adjacentLoc, _.copy(risk = possiblyBetterRisk, bestPathSoFar = adjacentLoc :: currentRiskAndPath.bestPathSoFar)), adjacentLoc :: newLocations)
          } else {
            (array, newLocations)
          }
        }
        val newState = State(updatedArray, restOfQueue.enqueueAll(newLocationsToConsider))
        (newState, newState)
      }
    }

    val finalState = states.last
    finalState.array2d.get(Loc(array2d.width, array2d.height))



  case class State(array2d: Array2D[RiskAndPath],
                   locationsToCheck: Queue[Loc])


  case class RiskAndPath(value: Int, risk: Int, bestPathSoFar: List[Loc]) {

    override def toString: String = "[%02d %02d %02d]".format(value, risk, bestPathSoFar.size)

  }

  object RiskAndPath {
    // Initialise all locations with the maximum possible risk, that way the first path we try for them will
    // automatically be the best so far..
    def apply(value: Int): RiskAndPath = RiskAndPath(value, Integer.MAX_VALUE, List.empty)
  }
}
