package uk.co.danielrendall.adventofcode.y2024

import uk.co.danielrendall.adventofcode.utils.ArrayUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day4 {

  val testData: LazyList[String] =
    """MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2024/day4.txt").lines

  @tailrec
  def has(array: Array2D[Char],
          loc: Loc,
          word: List[Char],
          dir: CompassDirection): Boolean = {
    word match
      case head :: tail =>
        if (array.contains(loc) && loc.get(array) == head) {
          has(array, dir.op(loc), tail, dir)
        } else {
          false
        }
      case _ =>
        true
  }


  @main def d4p1(): Unit = {
    val xmas: List[Char] = "XMAS".toList

    def solve(list: LazyList[String]) =
      val array: Array2D[Char] = buildBorderedArray(list, identity, ' ')
      array.locs.map { loc =>
        CompassDirection.all.count(d => has(array, loc, xmas, d))
      }.sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d4p2(): Unit = {
    val as: List[Char] = "AS".toList
    val am: List[Char] = "AM".toList

    def solve(list: LazyList[String]) =
      val array: Array2D[Char] = buildBorderedArray(list, identity, ' ')
        array.locs.map { loc =>
          // MAS
          val directionsOfMa: Seq[CompassDirection] =
            CompassDirection.diagonal.filter(d => has(array, loc, am, d) && has(array, loc, as, d.opposite))
          CompassDirection.diagonal.count(d => directionsOfMa.contains(d) && directionsOfMa.contains(d.right))
        }.sum


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }
}

