package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day1 {

  val testData: LazyList[String] =
    """199
      |200
      |208
      |210
      |200
      |207
      |240
      |269
      |260
      |263""".stripMargin.splitAndTrimToList

  val numbers: LazyList[Int] = this.getClass.getResourceAsStream("/2021/day1.txt").lines.filterNot(_.isEmpty).map(_.toInt)

  @main def d1p1(): Unit =
    val answer = numbers.zip(numbers.tail).count { case (f, s) => f < s }
    println(answer)

  @main def d1p2(): Unit =
    val windowSize = 3
    val windowedList = numbers.windowed(3).map(_.sum)
    val answer = windowedList.zip(windowedList.tail).count { case (f, s) => f < s }
    println(answer)
}
