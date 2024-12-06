package uk.co.danielrendall.adventofcode.y2024

import uk.co.danielrendall.adventofcode.utils.ArrayUtils
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.LinearDirection._
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.{Array2D, LinearDirection, Loc}
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day6 {

  val testData: LazyList[String] =
    """....#.....
      |.........#
      |..........
      |..#.......
      |.......#..
      |..........
      |.#..^.....
      |........#.
      |#.........
      |......#...""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2024/day6.txt").lines

  def traverse(current: LocDir, grid: Array2D[Char], visited: List[LocDir], visitSet: Set[LocDir]): List[LocDir] =
    val next = current.next
    if (visitSet.contains(next)) {
      throw LoopException
    }
    grid.get(next.loc) match {
      case ' ' =>
        visited.reverse
      case '#' =>
        // rotate and continue
        traverse(current.rotated, grid, visited, visitSet)
      case _ => // '.' or '^'
        traverse(next, grid, next :: visited, visitSet + next)
    }

  @main def d6p1(): Unit = {
    def solve(list: LazyList[String]) =
      val array: Array2D[Char] = ArrayUtils.buildBorderedArray[Char](list, identity, ' ')
      val startPos = LocDir(array.findLocs(_ == '^').head, Up)
      val path = traverse(startPos, array, List(startPos), Set(startPos))
      path.map(_.loc).toSet.size

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d6p2(): Unit = {

    def solve(list: LazyList[String]) =
      val array: Array2D[Char] = ArrayUtils.buildBorderedArray[Char](list, identity, ' ')
      val startPos = LocDir(array.findLocs(_ == '^').head, Up)
      val regularPath = traverse(startPos, array, List(startPos), Set(startPos))
      val positions = regularPath.tail.map(_.loc).distinct.filter { poss =>
        array.set(poss, '#')
        val ret = try {
          traverse(startPos, array, List(startPos), Set(startPos))
          false
        } catch {
          case LoopException =>
            true
        }
        array.set(poss, '.')
        ret
      }
      positions.size
    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  case class LocDir(loc: Loc, dir: LinearDirection) {
    def next: LocDir = LocDir(loc(dir.move), dir)

    def rotated: LocDir = LocDir(loc, dir.right)
  }

  case object LoopException extends Exception
}

