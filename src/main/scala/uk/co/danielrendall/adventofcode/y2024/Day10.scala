package uk.co.danielrendall.adventofcode.y2024

import uk.co.danielrendall.adventofcode.utils.ArrayUtils
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.{Array2D, LinearDirection, Loc}
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec
import scala.collection.mutable

object Day10 {

  val testData: LazyList[String] =
    """89010123
      |78121874
      |87430965
      |96549874
      |45678903
      |32019012
      |01329801
      |10456732""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2024/day10.txt").lines

  val digits: List[Char] = "9876543210".toList
  val pairs: List[(Char, Char)] = digits.zip(digits.tail)

  def solvePart1(array: ArrayUtils.Array2D[Char],
                 locArray: ArrayUtils.Array2D[mutable.Set[ArrayUtils.Loc]]): Int = {
    val ends = array.findLocs(_ == '9')
    ends.foreach(loc => locArray.get(loc).add(loc))
    @tailrec
    def process(remaining: List[(Char, Char)]): Unit = remaining match
      case head :: rest =>
        val first = head._1
        val second = head._2
        array.findLocs(_ == first).foreach { loc =>
          val locsReachableFromHere = locArray.get(loc)
          if (locsReachableFromHere.nonEmpty) {
            LinearDirection.all.foreach { dir =>
              val locInDirection = dir(loc)
              if (array.get(locInDirection) == second) {
                locArray.get(locInDirection).addAll(locsReachableFromHere)
              }
            }
          }
        }
        process(rest)
      case _ =>
        ()
    process(pairs)
    val starts = array.findLocs(_ == '0')
    starts.map(loc => locArray.get(loc).size).sum
  }

  def solvePart2(array: ArrayUtils.Array2D[Char],
                 countArray: ArrayUtils.Array2D[Int]): Int = {

    def inc(loc: Loc, amount: Int): Unit =
      countArray.set(loc, countArray.get(loc) + amount)


    val ends = array.findLocs(_ == '9')
    ends.foreach(loc => inc(loc, 1))
    @tailrec
    def process(remaining: List[(Char, Char)]): Unit = remaining match
      case head :: rest =>
        val first = head._1
        val second = head._2
        array.findLocs(_ == first).foreach { loc =>
          val countToLoc = countArray.get(loc)
          LinearDirection.all.foreach { dir =>
            val locInDirection = dir(loc)
            if (array.get(locInDirection) == second) {
              inc(locInDirection, countToLoc)
            }
          }
        }
        process(rest)
      case _ =>
        ()
    process(pairs)
    val starts = array.findLocs(_ == '0')
    starts.map(loc => countArray.get(loc)).sum
  }


  @main def d10p1(): Unit = {
    def solve(list: LazyList[String]) =
      val array = ArrayUtils.buildBorderedArray(list, identity, ' ')
      val locArray: Array2D[mutable.Set[Loc]] = ArrayUtils.buildEmptyArray(array.width, array.height, (l: Loc) => new mutable.HashSet[Loc](), new mutable.HashSet[Loc]())
      solvePart1(array, locArray)


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d10p2(): Unit = {
    def solve(list: LazyList[String]) =
      val array = ArrayUtils.buildBorderedArray(list, identity, ' ')
      val countArray: Array2D[Int] = ArrayUtils.buildEmptyArray(array.width, array.height, (l: Loc) => 0, 0)
      solvePart2(array, countArray)

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }
}

