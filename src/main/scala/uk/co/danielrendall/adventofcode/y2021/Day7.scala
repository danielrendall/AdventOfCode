package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.ComputingMap
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day7 {

  val testData: String = "16,1,2,0,4,2,7,1,2,14"

  val data: String = this.getClass.getResourceAsStream("/2021/day7.txt").lines.filterNot(_.isEmpty).head

  @main def d7p1() =
    def solve(data: String): Unit =
      solveWithScoreFunc(data, (a, b) => Math.abs(a - b))

    solve(testData)
    solve(data)

  @main def d7p2() =
    def solve(data: String): Unit =
      val cMap = new ComputingMap[Int, Int](x => x * (x+1) / 2)
      solveWithScoreFunc(data, (a, b) => cMap.get(Math.abs(a - b)))

    solve(testData)
    solve(data)

  def solveWithScoreFunc(data: String, scoreFunc: (Int, Int) => Int): Unit = {
    val counts = data.split(",").map(_.toInt).groupBy(identity).view.mapValues(_.length).toMap
    counts.keys.minMax match {
      case Some((min, max)) =>
        val seq: Seq[(Int, Int)] = (min to max).map { pos =>
          val score = counts.toList.map { case (otherPos, num) => scoreFunc(otherPos, pos) * num}.sum
          (pos, score)
        }
        val best = seq.minBy(_._2)
        println("Pos=" + best._1 + " Score=" + best._2)
      case None =>
        println("No data!")
    }

  }

}
