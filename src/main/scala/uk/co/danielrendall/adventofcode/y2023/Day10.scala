package uk.co.danielrendall.adventofcode.y2023

import uk.co.danielrendall.adventofcode.utils.ArrayUtils.Loc
import uk.co.danielrendall.adventofcode.utils.Move.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*
import uk.co.danielrendall.adventofcode.utils.{ArrayUtils, Move}

import scala.annotation.tailrec

object Day10 {

  val testData1: LazyList[String] =
    """7-F7-
      |.FJ|7
      |SJLL7
      ||F--J
      |LJ.LJ""".stripMargin.splitAndTrimToList

  val testData2: LazyList[String] =
    """.F----7F7F7F7F-7....
      |.|F--7||||||||FJ....
      |.||.FJ||||||||L7....
      |FJL7L7LJLJ||LJ.L-7..
      |L--J.L7...LJS7F-7L7.
      |....F-J..F7FJ|L7L7L7
      |....L7.F7||L7|.L7L7|
      |.....|FJLJ|FJ|F7|.LJ
      |....FJL-7.||.||||...
      |....L---J.LJ.LJLJ...""".stripMargin.splitAndTrimToList

  val testData3: LazyList[String] =
    """FF7FSF7F7F7F7F7F---7
      |L|LJ||||||||||||F--J
      |FL-7LJLJ||||||LJL-77
      |F--JF--7||LJLJ7F7FJ-
      |L---JF-JLJ.||-FJLJJ7
      ||F|F-JF---7F7-L7L|7|
      ||FFJF7L7F-JF7|JL---7
      |7-L-JL7||F7|L7F-7F7|
      |L.L7LFJ|||||FJL7||LJ
      |L7JLJL-JLJLJL--JLJ.L""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2023/day10.txt").lines

  val directionMap: Map[Char, Map[Move, Move]] = Map(
    '|' -> Map(up -> up, down -> down),
    '-' -> Map(right -> right, left -> left),
    'L' -> Map(down -> right, left -> up),
    'J' -> Map(down -> left, right -> up),
    '7' -> Map(up -> left, right -> down),
    'F' -> Map(up -> right, left -> down),
    'S' -> Map.empty,
    '.' -> Map.empty
  )

  @main def d10p1(): Unit = {
    def solve(list: LazyList[String]) = {
      val array = ArrayUtils.buildBorderedArray[Char](list, identity, '.')
      val start = array.findLocs(_ == 'S').head
      Array(up, down, left, right).find { initialMove =>
        directionMap(array.get(start(initialMove))).contains(initialMove)
      } match
        case Some(firstMove) =>
          getPath(array, start, firstMove)._1.length / 2
        case None =>
          throw new Exception("No first move possible")
    }


    println("Test: " + solve(testData1))
    println("Actual: " + solve(numbers))
  }

  @main def d10p2(): Unit = {

    def solve(list: LazyList[String]): Int = {
      val array = ArrayUtils.buildBorderedArray[Char](list, identity, '.')
      val start = array.findLocs(_ == 'S').head
      Array(up, down, left, right).find { initialMove =>
        directionMap(array.get(start(initialMove))).contains(initialMove)
      } match {
        case Some(firstMove) =>
          val (moves, locs) = getPath(array, start, firstMove)
          // We need to turn the initial 'S' into the actual pipe section
          val lastMove = moves.last // The move that took us back to the start
          directionMap.find { case (_, map) => map.get(lastMove).contains(firstMove)}.foreach { case (char, _) =>
            array.set(start, char)
          }

          val locSet = locs.toSet
          array.rowIteratorLR.map { list =>
            @tailrec
            def countInteriorLocs(remaining: List[Loc], count: Int, outside: Boolean): Int = {
              remaining match {
                case head :: rest =>
                  if (locSet.contains(head)) {
                    array.get(head) match
                      case '|' | 'L' | 'J' => countInteriorLocs(rest, count, !outside)
                      case _ => countInteriorLocs(rest, count, outside)
                  } else {
                    countInteriorLocs(rest, if (outside) count else count + 1, outside)
                  }

                case _ =>
                  count
              }
            }

            countInteriorLocs(list.toList, 0, true)
          }.sum
        case None =>
          throw new Exception("No first move possible")
      }
    }

    println("Test1: " + solve(testData2))
    println("Test2: " + solve(testData3))
    println("Actual: " + solve(numbers))
  }

  private def getPath(array: ArrayUtils.Array2D[Char], start: Loc, firstMove: Move): (List[Move], List[Loc]) = {

    @tailrec
    def tracePath(current: Loc, moves: List[Move], locs: List[Loc]): (List[Move], List[Loc]) =
      if (current == start) {
        (moves.reverse, locs.reverse)
      } else {
        directionMap(array.get(current)).get(moves.head) match
          case Some(nextDirection) =>
            tracePath(current(nextDirection), nextDirection :: moves, current :: locs)
          case None =>
            throw new Exception(s"At loc $current after ${moves.head} there was no next move")

      }

    tracePath(start(firstMove), List(firstMove), List(start))
  }

}

