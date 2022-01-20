package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day6 {

  /**
   * Lanternfish breeding. A lanternfish creates a new fish in a cycle of length 7; after every 7 days, it creates a new
   * Lanternfish. However, the _first_ cycle for each lanternfish has length 9, and 7 thereafter. Fish are modelled by
   * numbers, giving the number of days remaining in their current cycle. This decrements every day until it hits 0.
   * The following day after hitting 0, the days remaining resets to 6, and a new lanternfish with 8 days remaining is
   * created.
   *
   * Part 1
   * Given these rules, and the starting data, how many fish are there after 80 days.
   *
   * Part 2
   * How many lanternfish after 256 days?
   */

  val testData: String = "3,4,3,1,2"

  val data: String = this.getClass.getResourceAsStream("/2021/day6.txt").lines.filterNot(_.isEmpty).head

  @main def d6p1(): Unit =
    def solve(str: String): Int =

      // Map from number of days remaining in cycle -> number of fish with that number of days remaining. This
      // represents the state of the system
      val counts: Map[Int, Int] = str.split(",").map(_.toInt).sorted.groupBy(identity).view.mapValues(_.length).toMap

      /**
       * @param daysRemaining Number of days left for simulation to run. If 0, simulation is over.
       * @param currentState Map as described above
       * @return
       */
      @tailrec
      def update(daysRemaining: Int, currentState: Map[Int, Int]): Map[Int, Int] =
        // If we've finished, return the final state
        if (daysRemaining == 0) currentState else {
          // We have to compute the next state based on this one. Any fish which have reached day 0 will return to day
          // 6 on the next day, and spawn a new fish whose timer starts at 8.
          val nextStateSeq: Seq[(Int, Int)] = currentState.toSeq.flatMap { case (days, num) =>
            if (days == 0) {
              Seq((6 -> num), (8 -> num))
            } else {
              Seq(((days - 1) -> num))
            }
          }
          // Note that our nextStateSeq may contain two pairs for day 6, representing those that have just returned to
          // day 6, and any that have descended from day 7 to day 6. So we need to add the sums. Although there's only
          // one day affected by this, it's straightforward to group all of the things by the day number, convert the
          // resulting sequences of (day, count) to just the counts, and then sum them.
          // Group by day num, and combine values
          val nextState: Map[Int, Int] = nextStateSeq.groupBy(_._1).view.mapValues(v => v.map(_._2).sum).toMap
          update(daysRemaining - 1, nextState)
        }
      val finalState = update(80, counts)
      finalState.values.sum

    println("Test: " +  solve(testData))
    println("Actual: " + solve(data))

  @main def d6p2(): Unit =
    def solve(str: String): BigInt =

      // The numbers get big in this case; we need to use BigInt
      val counts: Map[Int, BigInt] =
        str.split(",").map(_.toInt).sorted.groupBy(identity).view.mapValues(s => BigInt(s.length)).toMap

      // My original solution was just a copy of the part 1 solution with "Int" changed to "BigInt", but for a bit of
      // variety, let's use LazyList.unfold. This takes an initial state and a function which, given the current state,
      // will compute the next state and also possibly some external result. Here, the next state is the map of
      // days remaining -> fish count, the result is the total number of fish

      val stream: Seq[BigInt] = LazyList.unfold(counts) { currentState =>
        val nextState: Map[Int, BigInt] = currentState.toSeq.flatMap { case (days, num) =>
          if (days == 0) {
            Seq((6 -> num), (8 -> num))
          } else {
            Seq(((days - 1) -> num))
          }
        }.groupBy(_._1).view.mapValues(v => v.map(_._2).sum).toMap
        Some((nextState.values.sum, nextState))
      }

      stream.drop(255).head

    println("Test: " +  solve(testData))
    println("Actual: " + solve(data))

}
