package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day3 {

  val testData: LazyList[String] =
    """00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2021/day3.txt").lines.filterNot(_.isEmpty)

  @main def d3p1(): Unit =
    numbers.headOption match {
      case Some(initial) =>
        val counts: Seq[Int] = initial.map(_ => 0)
        val finalCounts = numbers.foldLeft(counts) { case (currentCounts, nextString) =>
          nextString.zip(currentCounts).map { case (c, i) => if (c == '1') i + 1 else i - 1 }
        }
        val finalGammaInBinary = finalCounts.map(i => if (i > 0) '1' else if (i < 0) '0' else 'F').mkString
        val finalEpsilonInBinary = finalGammaInBinary.map(c => if (c == '1') then '0' else '1').mkString

        val gamma = Integer.parseInt(finalGammaInBinary, 2)
        val epsilon = Integer.parseInt(finalEpsilonInBinary, 2)

        println("Gamma: " + gamma + " Epsilon: " + epsilon)
        println(gamma * epsilon)


      case None =>
      // nothing to do
    }

  @main def d3p2(): Unit =
    numbers.headOption match {
      case Some(initial) =>
        // This is all a bit horrible...
        val (mostPopularSoFar, leastPopularSoFar, mostPopular, leastPopular) =
          initial.foldLeft((List.empty[Char], List.empty[Char], numbers.toList, numbers.toList)) {
            case ((mostSoFar, leastSoFar, numbersForMost, numbersForLeast), _) =>

              val (newMostSoFar, newNumbersForMost) = if (numbersForMost.size > 1) {
                val thisCharForMost = numbersForMost.map(s => s.head)
                val mostPopularChar: Char = {
                  val count = thisCharForMost.foldLeft(0) { case (cur, c) => if (c == '1') cur + 1 else cur - 1 }
                  if (count >= 0) '1' else '0'
                }
                (mostPopularChar :: mostSoFar, numbersForMost.filter(_.head == mostPopularChar).map(_.tail))
              } else (mostSoFar, numbersForMost)

              val (newLeastSoFar, newNumbersForLeast) = if (numbersForLeast.size > 1) {
                val thisCharForLeast = numbersForLeast.map(s => s.head)
                val leastPopularChar = {
                  val count = thisCharForLeast.foldLeft(0) { case (cur, c) => if (c == '1') cur + 1 else cur - 1 }
                  if (count >= 0) '0' else '1'
                }
                (leastPopularChar :: leastSoFar, numbersForLeast.filter(_.head == leastPopularChar).map(_.tail))
              } else (leastSoFar, numbersForLeast)

              (newMostSoFar, newLeastSoFar, newNumbersForMost, newNumbersForLeast)
          }


        val mostBinary = mostPopularSoFar.reverse.mkString + mostPopular.head
        val leastBinary = leastPopularSoFar.reverse.mkString + leastPopular.head
        println("MOST = " + mostBinary)
        println("LEAST = " + leastBinary)
        val oxygen = Integer.parseInt(mostBinary, 2)
        val co2 = Integer.parseInt(leastBinary, 2)

        println("Oxygen: " + oxygen + " CO2: " + co2)
        println(oxygen * co2)


      case None =>
      // nothing to do
    }


}
