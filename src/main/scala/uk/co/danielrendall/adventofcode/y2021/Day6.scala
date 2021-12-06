package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day6 {

  val testData: String = "3,4,3,1,2"

  val data: String = this.getClass.getResourceAsStream("/2021/day6.txt").lines.filterNot(_.isEmpty).head

  @main def d6p1(): Unit =
    def solve(str: String): Unit =
      val counts: Map[Int, Int] = str.split(",").map(_.toInt).sorted.groupBy(identity).view.mapValues(_.length).toMap
      @tailrec
      def update(daysRemaining: Int, currentState: Map[Int, Int]): Map[Int, Int] =
        val sorted = currentState.toSeq.sortBy(_._1)
//        println(sorted.map { case(v, c) => c + "*" + v}.mkString(", "))
        if (daysRemaining == 0) currentState else {
          // careful: any 0 becomes a 6 at the next generation and generates an 8, but any 7s also become 6s
          val nextStateSeq = currentState.toSeq.flatMap { case (days, num) =>
            if (days == 0) {
              Seq((6 -> num), (8 -> num))
            } else {
              Seq(((days - 1) -> num))
            }
          }
          // Group by day num, and combine values
          val nextState: Map[Int, Int] = nextStateSeq.groupBy(_._1).view.mapValues(v => v.map(_._2).sum).toMap
          update(daysRemaining - 1, nextState)
        }
      val finalState = update(80, counts)
      val result = finalState.values.sum

      println(result)

    solve(testData)
    solve(data)

  @main def d6p2(): Unit =
    def solve(str: String): Unit =
      val counts: Map[Int, BigInt] = str.split(",").map(_.toInt).sorted.groupBy(identity).view.mapValues(s => BigInt(s.length)).toMap
      @tailrec
      def update(daysRemaining: Int, currentState: Map[Int, BigInt]): Map[Int, BigInt] =
        val sorted = currentState.toSeq.sortBy(_._1)
        //        println(sorted.map { case(v, c) => c + "*" + v}.mkString(", "))
        if (daysRemaining == 0) currentState else {
          // careful: any 0 becomes a 6 at the next generation and generates an 8, but any 7s also become 6s
          val nextStateSeq = currentState.toSeq.flatMap { case (days, num) =>
            if (days == 0) {
              Seq((6 -> num), (8 -> num))
            } else {
              Seq(((days - 1) -> num))
            }
          }
          // Group by day num, and combine values
          val nextState: Map[Int, BigInt] = nextStateSeq.groupBy(_._1).view.mapValues(v => v.map(_._2).sum).toMap
          update(daysRemaining - 1, nextState)
        }
      val finalState = update(256, counts)
      val result = finalState.values.sum

      println(result)

    solve(testData)
    solve(data)

}
