package uk.co.danielrendall.adventofcode.y2023

import uk.co.danielrendall.adventofcode.utils.ArrayUtils
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.Loc
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day3 {

  val testData: LazyList[String] =
    """467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2023/day3.txt").lines

  def getNumbersAndSymbols(list: LazyList[String]): LazyList[NumberAndLoc] = {
    val array = ArrayUtils.buildBorderedArray[Char](list, identity, '.')

    def buildNumberAndLoc(numStart: Loc, curNum: String) = {
      val locLeft = numStart.left
      val locUpLeft = numStart.upLeft
      val locDownLeft = numStart.downLeft
      val locRight = numStart.right(curNum.length) // this will be the cell after
      val locUpRight = locRight.up
      val locDownRight = locRight.down
      val allAdjacent: LazyList[Loc] = LazyList(locLeft, locUpLeft, locDownLeft, locRight, locUpRight, locDownRight) :++
        (0 until curNum.length).flatMap(x => LazyList(numStart.right(x).up, numStart.right(x).down))
      val symOpt: Option[SymbolAndLoc] = allAdjacent.map(l => (l, array.get(l))).find(s => s._2 != '.').map { case (loc, char) => SymbolAndLoc(char, loc) }
      NumberAndLoc(curNum.toInt, numStart, symOpt)
    }

    array.rowIteratorLR.flatMap { locs =>

      @tailrec
      def findNums(rowLocs: List[Loc],
                   curNum: String,
                   lastNumStartOpt: Option[Loc],
                   nums: List[NumberAndLoc]): List[NumberAndLoc] = {
        rowLocs match {
          case head :: rest =>
            val char = array.get(head)
            lastNumStartOpt match {
              case Some(lastNumStart) =>
                // In a number...
                if (char.isDigit) {
                  // still in a number
                  findNums(rest, curNum + char, lastNumStartOpt, nums)
                } else {
                  // Not in a number any more
                  findNums(rest, "", None, buildNumberAndLoc(lastNumStart, curNum) :: nums)
                }
              case None =>
                // Not in a number
                if (char.isDigit) {
                  // Now in a number!
                  findNums(rest, s"$char", Some(head), nums)
                } else {
                  // Still not in a number
                  findNums(rest, "", None, nums)
                }
            }
          case _ =>
            lastNumStartOpt match
              case Some(lastNumStart) =>
                (buildNumberAndLoc(lastNumStart, curNum) :: nums).reverse
              case _ =>
                nums.reverse
        }

      }

      findNums(locs.toList, "", None, List.empty)

    }
  }

  @main def d3p1(): Unit = {
    def solve(list: LazyList[String]) = {
      getNumbersAndSymbols(list).filter(_.sym.isDefined).map(_.number).sum
    }


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d3p2(): Unit = {

    def solve(list: LazyList[String]) = {
      getNumbersAndSymbols(list).filter(_.sym.exists(_.symbol == '*'))
        .groupBy(_.sym.get.loc)
        .filter { case (loc, group) => group.size == 2}
        .map { case (loc, group) => group.map(_.number).product}
        .sum
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  case class SymbolAndLoc(symbol: Char, loc: Loc)

  case class NumberAndLoc(number: Int, numStartLoc: Loc, sym: Option[SymbolAndLoc])
}

