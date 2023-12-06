package uk.co.danielrendall.adventofcode.y2023

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day6 {

  val testData: LazyList[String] =
    """Time:      7  15   30
      |Distance:  9  40  200""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2023/day6.txt").lines

  @main def d6p1(): Unit = {

    def parse(data: Seq[String]): List[Race] = {
      val time = data(0).split(':')(1).trim.split("\\s+").map(_.toInt)
      val distance = data(1).split(':')(1).trim.split("\\s+").map(_.toInt)
      time.zip(distance).map(x => Race(x._1, x._2)).toList
    }

    def solve(list: LazyList[String]) =
      parse(list).map(_.optionCount).product


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d6p2(): Unit = {

    def solve(list: LazyList[String]) = {
      val time = list(0).split(':')(1).trim.filter(_.isDigit).toLong
      val distance = list(1).split(':')(1).trim.filter(_.isDigit).toLong
      Race(time, distance).optionCount
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }


  case class Race(time: Long, distance: Long) {

    def roots: (Double, Double) = {
      val disc = Math.sqrt(time * time - 4 * distance)
      ((time - disc) / 2.0, (time + disc) / 2.0)
    }

    def optionCount: Long =
      val (lower, upper) = roots
      val lowerCeil = (if (lower.toInt.toDouble == lower) lower + 1.0 else lower.ceil).toLong
      val upperFloor = (if (upper.toInt.toDouble == upper) upper - 1.0 else upper.floor).toLong
      upperFloor + 1 - lowerCeil
  }
}

