package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.ArrayUtils
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.*
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day9 {

  val testData: LazyList[String] =
    """2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678""".stripMargin.splitAndTrimToList

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day9.txt").lines.filterNot(_.isEmpty)

  @main def d9p1() =
    def solve(seq: LazyList[String]): Unit =

      val array2d = buildBorderedArray[Int](seq, c => s"$c".toInt, 9)

      val lowPoints: Seq[Height] = getLowPoints(array2d)
      println("Low points found: " + lowPoints.size)

      val score = lowPoints.map(_.v + 1).sum

      println(score)
    solve(testData)
    solve(data)

  @main def d9p2() =
    def solve(seq: LazyList[String]): Unit =

      val array2d = buildBorderedArray[Int](seq, c => s"$c".toInt, 9)
      val lowPoints: Seq[Height] = getLowPoints(array2d)

      def height(loc: Loc) = Height(loc, loc.get(array2d))

      @tailrec
      def findBasins(remainingLowPoints: List[Height], currentBasins: List[Set[Height]]): List[Set[Height]] =
        remainingLowPoints match {
          case head :: rest =>
            @tailrec
            def findBasin(HeightsAddedLastTime: Set[Height], heightsInBasin: Set[Height]): Set[Height] =
              if (HeightsAddedLastTime.isEmpty)
                heightsInBasin
              else
                val newPoints = HeightsAddedLastTime.flatMap { p =>
                  Set(
                    height(p.loc.up),
                    height(p.loc.down),
                    height(p.loc.left),
                    height(p.loc.right)
                  ).filterNot(_.v == 9)
                    .filter(_.v > p.v)
                    .diff(heightsInBasin)
                }
                findBasin(newPoints, heightsInBasin ++ newPoints)

            val basinForPoint = findBasin(Set(head), Set(head))
            findBasins(remainingLowPoints.filterNot(basinForPoint.contains), basinForPoint :: currentBasins)

          case _ => currentBasins
        }

      val basins = findBasins(lowPoints.toList, List.empty)
      println("Total basins found: " + basins.size)

      val result = basins.map(_.size).sorted(Ordering[Int].reverse).take(3).product
      println(result)

    solve(testData)
    solve(data)


  private def getLowPoints(array2D: Array2D[Int]): Seq[Height] =
    array2D.locs.flatMap { loc =>
      val value = loc.get(array2D)
      lazy val up = loc.up.get(array2D)
      lazy val down = loc.down.get(array2D)
      lazy val left = loc.left.get(array2D)
      lazy val right = loc.right.get(array2D)
      if ((value < up) && (value < down) && (value < left) && (value < right)) Some(Height(loc, value)) else None
    }

  case class Height(loc: Loc, v: Int)
}
