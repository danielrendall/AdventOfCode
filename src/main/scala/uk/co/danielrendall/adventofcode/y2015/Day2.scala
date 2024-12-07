package uk.co.danielrendall.adventofcode.y2015


import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.util.matching.Regex

object Day2 {

  val testData: LazyList[String] =
    """2x3x4
      |1x1x10""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2015/day2.txt").lines

  @main def d2p1(): Unit = {
    def solve(list: LazyList[String]) =
      list.map(Present.apply).map(_.paperTotal).sum


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d2p2(): Unit = {

    def solve(list: LazyList[String]) =
      list.map(Present.apply).map(_.ribbonTotal).sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  case class Present(l: Int, w: Int, h: Int) {

    val areas: Seq[Int] = Seq(l * w, w * h, h * l)
    val paperTotal: Int = areas.sum * 2 + areas.min

    val perimeters: Seq[Int] = Seq(2 * (l + w), 2 * (w + h), 2 * (h + l))

    val ribbonTotal: Int = perimeters.min + (l * w * h)
  }

  object Present {

    val regex: Regex = "^([0-9]+)x([0-9]+)x([0-9]+)$".r

    def apply(s: String): Present = s match
      case regex(l, w, h) => Present(l.toInt, w.toInt, h.toInt)

  }
}

