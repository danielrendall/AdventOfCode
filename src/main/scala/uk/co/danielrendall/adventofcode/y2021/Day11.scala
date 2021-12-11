package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.ArrayUtils
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.*
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*
import uk.co.danielrendall.adventofcode.utils.AnyUtils.*

import scala.annotation.tailrec

object Day11 {

  val testData: LazyList[String] =
    """5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526""".stripMargin.splitAndTrimToList

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day11.txt").lines.filterNot(_.isEmpty)

  @main def d11p1() =
    def solve(seq: LazyList[String]): Unit =

      val initial: State = State(buildBorderedArray[Int](seq, c => s"$c".toInt, 0), 0)

      val trajectory = LazyList.unfold(initial)(state => Option(state.advance.tuple2))

      val finalBoard = trajectory.drop(99).head

      println(finalBoard.board)
      println
      println(finalBoard.flashes)
      println

    solve(testData)
    solve(data)

  @main def d11p2() =
    def solve(seq: LazyList[String]): Unit =

      val initial: State = State(buildBorderedArray[Int](seq, c => s"$c".toInt, 0), 0)

      val trajectory = LazyList.unfold(initial)(state => Option(state.advance.tuple2))

      val first = trajectory.zipWithIndex.find { case (state, idx) => state.isAllZeroes }.get._2 + 1

      println(first)

    solve(testData)
    solve(data)




  case class State(board: Array2D[Int], flashes: Int) {
    def advance: State =
      @tailrec
      def redistributeUntilNoChange(currentState: State): State =
        val newState = currentState.redistribute
        if (newState.flashes == currentState.flashes) newState
        else redistributeUntilNoChange(newState)

      redistributeUntilNoChange(addOneUnit)

    def isAllZeroes: Boolean = board.locs.forall(_.get(board) == 0)

    // Add one unit of energy everywhere; this may leave the board in an inconsistent state
    private def addOneUnit: State = State(board.map(l => l.get(board) + 1), flashes)

    // Iterate through the locations on the board. Any location with energy > 9 counts as a flash and the energy is
    // reset to zero (and plays no further part in this round). Any location with energy = 0 is ignored (must have
    // flashed already). Any other location may acquire energy from something adjacent which flashed (i.e. anything
    // adjacent which currently has energy > 9, because we know that will flash)
    // We return the new state (which might be the same as this) and a Boolean indicating if anything changed

    private def redistribute: State = {
      val oldZeroes = board.locs.count(_.get(board) == 0)

      val newBoard = board.map { loc =>
        loc.get(board) match {
          case 0 =>
            // 0s are unchanged
            0
          case i if i<10 =>
            i + (loc.allAdjacent.map(_.get(board)).count(_ > 9))
          case _ =>
            // Flash!
            0
        }
      }

      val newZeroes = newBoard.locs.count(_.get(newBoard) == 0)
      val newFlashes = newZeroes - oldZeroes

      State(newBoard, flashes + newFlashes)
    }
  }
}
