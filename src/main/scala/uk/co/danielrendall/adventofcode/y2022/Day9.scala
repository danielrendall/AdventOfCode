package uk.co.danielrendall.adventofcode.y2022

import uk.co.danielrendall.adventofcode.utils.ArrayUtils.Loc
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*
import uk.co.danielrendall.adventofcode.y2022.Day9.UpRight

import scala.util.matching.Regex

object Day9 {

  val testData: LazyList[String] =
    """R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2""".stripMargin.splitAndTrimToList

  val testData2: LazyList[String] =
    """R 5
      |U 8
      |L 8
      |D 3
      |R 17
      |D 10
      |L 25
      |U 20""".stripMargin.splitAndTrimToList

  val realData: LazyList[String] = this.getClass.getResourceAsStream("/2022/day9.txt").lines.filterNot(_.isEmpty)

  @main def d9p1(): Unit =
    def solve(list: LazyList[String]) =
      val singleHeadMoves: LazyList[Instruction] = list.flatMap(toInstructionSequence)
      val tailMoves = LazyList.unfold[Option[Instruction], TailState](TailState(singleHeadMoves, 0, 0))(nextState).flatten
      val (allVisited, finalLoc) = tailMoves.foldLeft((Set[Loc](Loc(0, 0)), Loc(0, 0))) { case ((visited, current), move) =>
        val nextLocation = Loc(current.x + move.dx, current.y + move.dy)
        (visited + nextLocation, nextLocation)
      }
      allVisited.size

    println("Test: " + solve(testData))
    println("Actual: " + solve(realData))

  @main def d9p2(): Unit =
    def solve(list: LazyList[String]) =
      val singleHeadMoves: LazyList[Instruction] = list.flatMap(toInstructionSequence)
      val knot1Moves = LazyList.unfold[Option[Instruction], TailState](TailState(singleHeadMoves, 0, 0))(nextState).flatten
      val knot2Moves = LazyList.unfold[Option[Instruction], TailState](TailState(knot1Moves, 0, 0))(nextState).flatten
      val knot3Moves = LazyList.unfold[Option[Instruction], TailState](TailState(knot2Moves, 0, 0))(nextState).flatten
      val knot4Moves = LazyList.unfold[Option[Instruction], TailState](TailState(knot3Moves, 0, 0))(nextState).flatten
      val knot5Moves = LazyList.unfold[Option[Instruction], TailState](TailState(knot4Moves, 0, 0))(nextState).flatten
      val knot6Moves = LazyList.unfold[Option[Instruction], TailState](TailState(knot5Moves, 0, 0))(nextState).flatten
      val knot7Moves = LazyList.unfold[Option[Instruction], TailState](TailState(knot6Moves, 0, 0))(nextState).flatten
      val knot8Moves = LazyList.unfold[Option[Instruction], TailState](TailState(knot7Moves, 0, 0))(nextState).flatten
      val knot9Moves = LazyList.unfold[Option[Instruction], TailState](TailState(knot8Moves, 0, 0))(nextState).flatten
      val (allVisited, finalLoc) = knot9Moves.foldLeft((Set[Loc](Loc(0, 0)), Loc(0, 0))) { case ((visited, current), move) =>
        val nextLocation = Loc(current.x + move.dx, current.y + move.dy)
        (visited + nextLocation, nextLocation)
      }
      allVisited.size

    println("Test: " + solve(testData))
    println("Test2: " + solve(testData2))
    println("Actual: " + solve(realData))

  // Keep track of where the tail is relative to the head
  case class TailState(remainingHeadMoves: LazyList[Instruction], relX: Int, relY: Int)

  def nextState(ts: TailState): Option[(Option[Instruction], TailState)] = {
    ts.remainingHeadMoves.headOption.map { headMove =>

      val makeTailState: (Int, Int) => TailState = TailState(ts.remainingHeadMoves.tail, _, _)

      // Tail moves relative to the head in the opposite direction
      val newRelX = ts.relX - headMove.dx
      val newRelY = ts.relY - headMove.dy


      def move(tailMove: Instruction): (Option[Instruction], TailState) =
        (Some(tailMove), makeTailState(newRelX + tailMove.dx, newRelY + tailMove.dy))

      // Given the constraint that the tail should never be more than 1 step from the head, if any of the new
      // relative coordinates is -2 or 2, we'll need to move and emit a move
      (newRelX, newRelY) match
        case (2, 0) => move(Left)
        case (2, 1) => move(DownLeft)
        case (2, 2) => move(DownLeft)
        case (1, 2) => move(DownLeft)
        case (0, 2) => move(Down)
        case (-1, 2) => move(DownRight)
        case (-2, 2) => move(DownRight)
        case (-2, 1) => move(DownRight)
        case (-2, 0) => move(Right)
        case (-2, -1) => move(UpRight)
        case (-2, -2) => move(UpRight)
        case (-1, -2) => move(UpRight)
        case (0, -2) => move(Up)
        case (1, -2) => move(UpLeft)
        case (2, -2) => move(UpLeft)
        case (2, -1) => move(UpLeft)
        case _ => (None, makeTailState(newRelX, newRelY))
    }
  }

  def toInstructionSequence(s: String): Seq[Instruction] = s match
    case InstructionRegex(direction, distance) => {
      val fill = Seq.fill[Instruction](distance.toInt)
      direction match {
        case "R" => fill(Right)
        case "L" => fill(Left)
        case "U" => fill(Up)
        case "D" => fill(Down)
      }
    }

  val InstructionRegex: Regex = "([RLUD]) (\\d+)".r

  sealed abstract class Instruction(val dx: Int, val dy: Int)

  case object Right extends Instruction(1, 0)

  case object UpRight extends Instruction(1, 1)

  case object Up extends Instruction(0, 1)

  case object UpLeft extends Instruction(-1, 1)

  case object Left extends Instruction(-1, 0)

  case object DownLeft extends Instruction(-1, -1)

  case object Down extends Instruction(0, -1)

  case object DownRight extends Instruction(1, -1)


}
