package uk.co.danielrendall.adventofcode.y2022

import uk.co.danielrendall.adventofcode.utils.ArrayUtils.*
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day8 {

  val testData: Array2D[Int] =
    """30373
      |25512
      |65332
      |33549
      |35390""".stripMargin.splitAndTrimToList.toBorderedArray(c => s"$c".toInt, 0)

  val data: Array2D[Int] = this.getClass.getResourceAsStream("/2022/day8.txt")
    .lines.filterNot(_.isEmpty).toBorderedArray(c => s"$c".toInt, 0)

  @main def d8p1(): Unit =
    def solve(data: Array2D[Int]) = {
      def getVisibleLocs(toIt: Seq[Seq[Loc]]): Set[Loc] = {
        @tailrec
        def getLocs(remaining: Seq[Loc], found: List[Loc], highestSoFar: Int): List[Loc] = {
          remaining.headOption match
            case Some(loc) =>
              val height = data.get(loc)
              if (height > highestSoFar)
                getLocs(remaining.tail, loc :: found, height)
              else
                getLocs(remaining.tail, found, highestSoFar)

            case None => found
        }

        toIt.flatMap(seq => getLocs(seq, List.empty, -1)).toSet
      }

      (getVisibleLocs(data.rowIteratorLR) ++
        getVisibleLocs(data.rowIteratorRL) ++
        getVisibleLocs(data.columnIteratorTD) ++
        getVisibleLocs(data.columnIteratorDT)).size
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(data))

  @main def d8p2(): Unit =
    def solve(data: Array2D[Int]) = {
      val iterators: Seq[LazyList[LazyList[Loc]]] =
        Seq(data.rowIteratorLR, data.rowIteratorRL, data.columnIteratorTD, data.columnIteratorDT)

      def getViewingDistances(iterator: LazyList[LazyList[Loc]]): Array2D[Int] = {
        val distanceArray = Array2D.fill(data.width, data.height)(1)
        iterator.foreach { lineIt =>
          // Trees on the border can't see anything; model this by saying the distance to a tree of any height is zero
          val distancesToLastTree = Array.fill(10)(0)
          lineIt.foreach { loc =>
            val currentTreeHeight = data.get(loc)
            // Viewing distance is shortest distance to a tree of the current height or great
            val viewingDistance = distancesToLastTree.drop(currentTreeHeight).min
            distanceArray.set(loc, viewingDistance)
            // For next location, increase all of the distances by 1, except for the distance to the current tree height
            // which we reset to 1
            (0 until 10).foreach(idx => distancesToLastTree(idx) = distancesToLastTree(idx) + 1)
            distancesToLastTree(currentTreeHeight) = 1
          }
        }
        distanceArray
      }

      iterators.map(getViewingDistances).product.max
    }


    println("Test: " + solve(testData))
    println("Actual: " + solve(data))

}
