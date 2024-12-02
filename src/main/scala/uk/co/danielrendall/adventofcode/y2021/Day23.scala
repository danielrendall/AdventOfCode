package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*
import uk.co.danielrendall.adventofcode.y2021.Day22.*
import uk.co.danielrendall.adventofcode.y2021.Day23.GameState.{energyPerMove, expectedDestinations}

import scala.annotation.tailrec
import scala.collection.mutable

object Day23 {

  val testData: LazyList[String] =
    """#############
      |#...........#
      |###B#C#B#D###
      |  #A#D#C#A#
      |  #########""".stripMargin.splitToList

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day23.txt").lines

  @main def d23p1() =
    def solve(seq: Seq[String]): Unit =
      val initialState = GameState.parse(seq.take(4) ++ Seq("  #A#B#C#D#", "  #A#B#C#D#") ++ seq.slice(4, 5))

      val energy = solveFromInitialState(initialState)

      println(energy)

    solve(testData)
    solve(data)

  @main def d23p2() =
    def solve(seq: Seq[String]): Unit =
      val initialState = GameState.parse(seq.take(3) ++ Seq("  #D#C#B#A#", "  #D#B#A#C#") ++ seq.slice(3, 5))

      val energy = solveFromInitialState(initialState)

      println(energy)

    solve(testData)
    solve(data)




  def solveFromInitialState(initialState: GameState): Int =
    val testedAlready = new mutable.HashMap[String, Int]()
    @tailrec
    def search(statesToSearch: List[GameState],
               winningEnergy: Int): Int =
      statesToSearch match {
        case head :: rest =>
//          print("'" + head.toString + "' = " + head.energySpent + " Remaining: " + rest.size + " tested already " + testedAlready.size + " winning energy: " + winningEnergy + " - ")
          if (testedAlready.get(head.toString).exists(_ <= head.energySpent)) {
//            println("done better before")
            search(rest, winningEnergy)
          } else  if (head.energySpent >= winningEnergy) {
//            println("gone over budget")
            search(rest, winningEnergy)
          } else if (head.isWin) {
            println("found a win = " + head.energySpent)
            testedAlready.put(head.toString, head.energySpent)
            search(rest.filter(_.energySpent < head.energySpent), head.energySpent)
          } else {
            val movesToSearch = head.possibleMoves.filterNot(_.energySpent > winningEnergy).filterNot { poss =>
              testedAlready.get(poss.toString).exists(_ <= poss.energySpent)
            } ++ rest
            // Don't bother searching situations that are identical to ones we've queued reached with a lower
            // energy cost
//            println("adding more moves")
            testedAlready.put(head.toString, head.energySpent)
            search(movesToSearch, winningEnergy)
          }

        case _ => winningEnergy
      }

    search(List(initialState), Int.MaxValue)


  // Locations where things can be after a turn:
  // #############
  // #YY.Y.Y.Y.YY#
  // ###Y#Y#Y#Y###
  //   #Y#Y#Y#Y#
  //   #########
  //
  // Hallway positions with labels
  // # # # # # # # # # # # # #
  // # 0 1 . 2 . 3 . 4 . 5 6 #
  // # # # 7 # 8 # 9 # 10# # #
  //     # 11# 12# 13# 14#
  //     # # # # # # # # #
  // There are 14 positions where these things stop.

  // We never have to worry about moves along the hallway.
  //
  //

  case class Dest(idx: Int, moves: Int, requireClear: Set[Int]) {

    @inline def isInHallway: Boolean = Dest.isInHallway(idx)

  }

  object Dest {
    @inline def isInHallway(idx: Int): Boolean = (0 <= idx) && (idx <= 6)
  }

  case class GameState(override val toString: String,
                       energySpent: Int,
                       pos1HasBeenOccupied: Boolean,
                       pos5HasBeenOccupied: Boolean) {
    assert(toString.count(_ == 'A') == 4, s"$toString didn't have 4 As")
    assert(toString.count(_ == 'B') == 4, s"$toString didn't have 4 Bs")
    assert(toString.count(_ == 'C') == 4, s"$toString didn't have 4 Cs")
    assert(toString.count(_ == 'D') == 4, s"$toString didn't have 4 Ds")

    def isWin: Boolean = toString == "       ABCDABCDABCDABCD"


    def possibleMoves: List[GameState] = {
      val occupiedLocations: Seq[(Char, Int)] = toString.zipWithIndex.filterNot(_._1 == ' ')
      val occupiedIndexes = occupiedLocations.map(_._2).toSet

      occupiedLocations.flatMap { case (char, pos) =>
        val expectedDestinations: Seq[Int] = GameState.expectedDestinations(char)
        val positionInDestinations = expectedDestinations.indexOf(pos)

        def allDestinationsBelowIndexAreFilledWithChar(idx: Int) =
          expectedDestinations.zipWithIndex.filter(_._2 > idx).forall(s => toString(s._1) == char)


        if (positionInDestinations > -1 && allDestinationsBelowIndexAreFilledWithChar(positionInDestinations)) {
          // Already in final place, no point moving
          Seq.empty
        } else {
          // Get all locations that aren't blocked
          val unblockedLocations: Seq[Dest] =
            locations(pos).filterNot(dest => dest.requireClear.intersect(occupiedIndexes).nonEmpty).filter(d => toString(d.idx) == ' ')

          // If we're in the hallway to start with, we don't move except to our final destination
          val dontMoveAlongHallway = if (Dest.isInHallway(pos)) {
            unblockedLocations.filterNot(_.isInHallway)
          } else {
            unblockedLocations
          }

          val dontMoveIntoWrongDestination = dontMoveAlongHallway.filter { dest =>
            if (!dest.isInHallway) {
              // must be a destination
              val destPosInDestinations = expectedDestinations.indexOf(dest.idx)
              if (destPosInDestinations > -1 && allDestinationsBelowIndexAreFilledWithChar(destPosInDestinations)) {
                if ((pos == 0 && !pos1HasBeenOccupied) || (pos == 6 && !pos5HasBeenOccupied)) {
                  false
                } else {
                  true
                }
              } else {
                false
              }
            } else {
              true
            }
          }

          dontMoveIntoWrongDestination.map { dest =>
            val charArray = toString.toCharArray
            charArray(pos) = ' '
            charArray(dest.idx) = char
            GameState(charArray.mkString(""),
              energySpent + dest.moves * energyPerMove(char),
              pos1HasBeenOccupied || charArray(1) != ' ',
              pos5HasBeenOccupied || charArray(5) != ' ')
          }
        }


      }

    }.sortBy(_.energySpent).toList

    def lettersInBlockingPositions: Seq[Char] = Seq(toString(2), toString(3), toString(4)).filterNot(_ == ' ')
  }

  object GameState {
    def parse(data: Seq[String]): GameState =
      println("DATA = " + data.mkString("\n"))
      val array = "                       ".toCharArray
      data.slice(2, 6).map(_.filter(c => c == 'A' || c == 'B' || c == 'C' || c == 'D')).zip(Seq(7, 11, 15, 19)).flatMap { case (str, initialIdx) =>
        str.zipWithIndex.map { case (c, idx) => (c, idx + initialIdx) }
      }.foreach { case (c, idx) => array(idx) = c }
      GameState(array.mkString(""), 0, false, false)

    @inline def energyPerMove(c: Char): Int = c match {
      case 'A' => 1
      case 'B' => 10
      case 'C' => 100
      case 'D' => 1000
      case _ => throw new Exception("Bad char: " + c)
    }

    // Top, then bottom
    @inline def expectedDestinations(c: Char): Seq[Int] = c match {
      case 'A' => Seq(7, 11, 15, 19)
      case 'B' => Seq(8, 12, 16, 20)
      case 'C' => Seq(9, 13, 17, 21)
      case 'D' => Seq(10, 14, 18, 22)
      case _ => throw new Exception("Bad char: " + c)
    }
  }


  val locations: Map[Int, Seq[Dest]] = Seq(
    // Hallway
    Seq(
      7 ~ 3 `xx` "1",
      11 ~ 4 `xx` "1,7",
      15 ~ 5 `xx` "1,7,11",
      19 ~ 6 `xx` "1,7,11,15",
      8 ~ 5 `xx` "1,2",
      12 ~ 6 `xx` "1,2,8",
      16 ~ 7 `xx` "1,2,8,12",
      20 ~ 8 `xx` "1,2,812,26",
      9 ~ 7 `xx` "1,2,3",
      13 ~ 8 `xx` "1,2,3,9",
      17 ~ 9 `xx` "1,2,3,9,13",
      21 ~ 10 `xx` "1,2,3,9,13,17",
      10 ~ 9 `xx` "1,2,3,4",
      14 ~ 10 `xx` "1,2,3,4,10",
      18 ~ 11 `xx` "1,2,3,4,10,14",
      22 ~ 12 `xx` "1,2,3,4,10,14,18"
    ),
    Seq(
      7 ~ 2 `xx` "",
      11 ~ 3 `xx` "7",
      15 ~ 4 `xx` "7,11",
      19 ~ 5 `xx` "7,11,15",
      8 ~ 4 `xx` "2",
      12 ~ 5 `xx` "2,8",
      16 ~ 6 `xx` "2,8,12",
      20 ~ 7 `xx` "2,8,12,16",
      9 ~ 6 `xx` "2,3",
      13 ~ 7 `xx` "2,3,9",
      17 ~ 8 `xx` "2,3,9,13",
      21 ~ 9 `xx` "2,3,9,13,17",
      10 ~ 8 `xx` "2,3,4",
      14 ~ 9 `xx` "2,3,4,10",
      18 ~ 10 `xx` "2,3,4,10,14",
      22 ~ 11 `xx` "2,3,4,10,14,18"
    ),
    Seq(
      7 ~ 2 `xx` "",
      11 ~ 3 `xx` "7",
      15 ~ 4 `xx` "7,11",
      19 ~ 5 `xx` "7,11,15",
      8 ~ 2 `xx` "",
      12 ~ 3 `xx` "8",
      16 ~ 4 `xx` "8,12",
      20 ~ 5 `xx` "8,12,16",
      9 ~ 4 `xx` "3",
      13 ~ 5 `xx` "3,9",
      17 ~ 6 `xx` "3,9,13",
      21 ~ 7 `xx` "3,9,13,17",
      10 ~ 6 `xx` "3,4",
      14 ~ 7 `xx` "3,4,10",
      18 ~ 8 `xx` "3,4,10,14",
      22 ~ 9 `xx` "3,4,10,14,18"
    ),
    Seq(
      7 ~ 4 `xx` "2",
      11 ~ 5 `xx` "2,7",
      15 ~ 6 `xx` "2,7,11",
      19 ~ 7 `xx` "2,7,11,15",
      8 ~ 2 `xx` "",
      12 ~ 3 `xx` "8",
      16 ~ 4 `xx` "8,12",
      20 ~ 5 `xx` "8,12,16",
      9 ~ 2 `xx` "",
      13 ~ 3 `xx` "9",
      17 ~ 4 `xx` "9,13",
      21 ~ 5 `xx` "9,13,17",
      10 ~ 4 `xx` "4",
      14 ~ 5 `xx` "4,10",
      18 ~ 6 `xx` "4,10,14",
      22 ~ 7 `xx` "4,10,14,18"
    ),
    Seq(
      7 ~ 6 `xx` "2,3",
      11 ~ 7 `xx` "2,3,7",
      15 ~ 8 `xx` "2,3,7,11",
      19 ~ 9 `xx` "2,3,7,11,15",
      8 ~ 4 `xx` "3",
      12 ~ 5 `xx` "3,8",
      16 ~ 6 `xx` "3,8,12",
      20 ~ 7 `xx` "3,8,12,16",
      9 ~ 2 `xx` "",
      13 ~ 3 `xx` "9",
      17 ~ 4 `xx` "9,13",
      21 ~ 5 `xx` "9,13,17",
      10 ~ 2 `xx` "",
      14 ~ 3 `xx` "10",
      18 ~ 4 `xx` "10,14",
      22 ~ 5 `xx` "10,14,18"
    ),
    Seq(
      7 ~ 8 `xx` "2,3,4",
      11 ~ 9 `xx` "2,3,4,7",
      15 ~ 10 `xx` "2,3,4,7,11",
      19 ~ 11 `xx` "2,3,4,7,11,15",
      8 ~ 6 `xx` "3,4",
      12 ~ 7 `xx` "3,4,8",
      16 ~ 8 `xx` "3,4,8,12",
      20 ~ 9 `xx` "3,4,8,12,16",
      9 ~ 4 `xx` "4",
      13 ~ 5 `xx` "4,9",
      17 ~ 6 `xx` "4,9,13",
      21 ~ 7 `xx` "4,9,13,17",
      10 ~ 2 `xx` "",
      14 ~ 3 `xx` "10",
      18 ~ 4 `xx` "10,14",
      22 ~ 5 `xx` "10,14,18"
    ),
    Seq(
      7 ~ 9 `xx` "2,3,4,5",
      11 ~ 10 `xx` "2,3,4,5,7",
      15 ~ 11 `xx` "2,3,4,5,7,11",
      19 ~ 12 `xx` "2,3,4,5,7,11,15",
      8 ~ 7 `xx` "3,4,5",
      12 ~ 8 `xx` "3,4,5,8",
      16 ~ 9 `xx` "3,4,5,8,12",
      20 ~ 10 `xx` "3,4,5,8,12,16",
      9 ~ 5 `xx` "4,5",
      13 ~ 6 `xx` "4,5,9",
      17 ~ 7 `xx` "4,5,9,13",
      21 ~ 8 `xx` "4,5,9,13,17",
      10 ~ 3 `xx` "5",
      14 ~ 4 `xx` "5,10",
      18 ~ 5 `xx` "5,10,14",
      22 ~ 6 `xx` "5,10,14,18"
    ),
    // Top row
    Seq(
      0 ~ 3 `xx` "1",
      1 ~ 2 `xx` "",
      2 ~ 2 `xx` "",
      3 ~ 4 `xx` "2",
      4 ~ 6 `xx` "2,3",
      5 ~ 8 `xx` "2,3,4",
      6 ~ 9 `xx` "2,3,4,5",
    ),
    Seq(
      0 ~ 5 `xx` "1,2",
      1 ~ 4 `xx` "2",
      2 ~ 2 `xx` "",
      3 ~ 2 `xx` "",
      4 ~ 4 `xx` "3",
      5 ~ 6 `xx` "3,4",
      6 ~ 7 `xx` "3,4,5",
    ),
    Seq(
      0 ~ 7 `xx` "1,2,3",
      1 ~ 6 `xx` "2,3",
      2 ~ 4 `xx` "3",
      3 ~ 2 `xx` "",
      4 ~ 2 `xx` "",
      5 ~ 4 `xx` "4",
      6 ~ 5 `xx` "4,5",
    ),
    Seq(
      0 ~ 9 `xx` "1,2,3,4",
      1 ~ 8 `xx` "2,3,4",
      2 ~ 6 `xx` "3,4",
      3 ~ 4 `xx` "4",
      4 ~ 2 `xx` "",
      5 ~ 2 `xx` "",
      6 ~ 3 `xx` "5",
    ),
    // Second row
    Seq(
      0 ~ 4 `xx` "7,1",
      1 ~ 3 `xx` "7",
      2 ~ 3 `xx` "7",
      3 ~ 5 `xx` "7,2",
      4 ~ 7 `xx` "7,2,3",
      5 ~ 9 `xx` "7,2,3,4",
      6 ~ 10 `xx` "7,2,3,4,5",
    ),
    Seq(
      0 ~ 6 `xx` "8,1,2",
      1 ~ 5 `xx` "8,2",
      2 ~ 3 `xx` "8,",
      3 ~ 3 `xx` "8,",
      4 ~ 5 `xx` "8,3",
      5 ~ 7 `xx` "8,3,4",
      6 ~ 8 `xx` "8,3,4,5",
    ),
    Seq(
      0 ~ 8 `xx` "9,1,2,3",
      1 ~ 7 `xx` "9,2,3",
      2 ~ 5 `xx` "9,3",
      3 ~ 3 `xx` "9",
      4 ~ 3 `xx` "9",
      5 ~ 5 `xx` "9,4",
      6 ~ 6 `xx` "9,4,5",
    ),
    Seq(
      0 ~ 10 `xx` "10,1,2,3,4",
      1 ~ 9 `xx` "10,2,3,4",
      2 ~ 7 `xx` "10,3,4",
      3 ~ 5 `xx` "10,4",
      4 ~ 3 `xx` "10",
      5 ~ 3 `xx` "10",
      6 ~ 4 `xx` "10,5",
    ),
    // Third row
    Seq(
      0 ~ 5 `xx` "11,7,1",
      1 ~ 4 `xx` "11,7",
      2 ~ 4 `xx` "11,7",
      3 ~ 6 `xx` "11,7,2",
      4 ~ 8 `xx` "11,7,2,3",
      5 ~ 10 `xx` "11,7,2,3,4",
      6 ~ 11 `xx` "11,7,2,3,4,5",
    ),
    Seq(
      0 ~ 7 `xx` "12,8,1,2",
      1 ~ 6 `xx` "12,8,2",
      2 ~ 4 `xx` "12,8,",
      3 ~ 4 `xx` "12,8,",
      4 ~ 6 `xx` "12,8,3",
      5 ~ 8 `xx` "12,8,3,4",
      6 ~ 9 `xx` "12,8,3,4,5",
    ),
    Seq(
      0 ~ 9 `xx` "13,9,1,2,3",
      1 ~ 8 `xx` "13,9,2,3",
      2 ~ 6 `xx` "13,9,3",
      3 ~ 4 `xx` "13,9",
      4 ~ 4 `xx` "13,9",
      5 ~ 6 `xx` "13,9,4",
      6 ~ 7 `xx` "13,9,4,5",
    ),
    Seq(
      0 ~ 11 `xx` "14,10,1,2,3,4",
      1 ~ 10 `xx` "14,10,2,3,4",
      2 ~ 8 `xx` "14,10,3,4",
      3 ~ 6 `xx` "14,10,4",
      4 ~ 4 `xx` "14,10",
      5 ~ 4 `xx` "14,10",
      6 ~ 5 `xx` "14,10,5",
    ),
    // Fourth row
    Seq(
      0 ~ 6 `xx` "15,11,7,1",
      1 ~ 5 `xx` "15,11,7",
      2 ~ 5 `xx` "15,11,7",
      3 ~ 7 `xx` "15,11,7,2",
      4 ~ 9 `xx` "15,11,7,2,3",
      5 ~ 12 `xx` "15,11,7,2,3,4",
      6 ~ 13 `xx` "15,11,7,2,3,4,5",
    ),
    Seq(
      0 ~ 8 `xx` "16,12,8,1,2",
      1 ~ 7 `xx` "16,12,8,2",
      2 ~ 5 `xx` "16,12,8,",
      3 ~ 5 `xx` "16,12,8,",
      4 ~ 7 `xx` "16,12,8,3",
      5 ~ 9 `xx` "16,12,8,3,4",
      6 ~ 10 `xx` "16,12,8,3,4,5",
    ),
    Seq(
      0 ~ 10 `xx` "17,13,9,1,2,3",
      1 ~ 9 `xx` "17,13,9,2,3",
      2 ~ 7 `xx` "17,13,9,3",
      3 ~ 5 `xx` "17,13,9",
      4 ~ 5 `xx` "17,13,9",
      5 ~ 7 `xx` "17,13,9,4",
      6 ~ 8 `xx` "17,13,9,4,5",
    ),
    Seq(
      0 ~ 12 `xx` "18,14,10,1,2,3,4",
      1 ~ 11 `xx` "18,14,10,2,3,4",
      2 ~ 9 `xx` "18,14,10,3,4",
      3 ~ 7 `xx` "18,14,10,4",
      4 ~ 5 `xx` "18,14,10",
      5 ~ 5 `xx` "18,14,10",
      6 ~ 6 `xx` "18,14,10,5",
    )
  ).zipWithIndex.map { case (destinations, index) => index -> destinations }.toMap

  extension (int: Int)
    def ~(other: Int) = Dest(int, other, Set.empty)

  extension (dest: Dest)
    def xx(blocks: String) = Dest(dest.idx, dest.moves, dest.requireClear ++ blocks.split(",").filterNot(_.isEmpty).map(_.toInt))

}
