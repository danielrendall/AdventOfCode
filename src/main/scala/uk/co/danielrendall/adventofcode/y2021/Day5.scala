package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.CountingMap
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.util.matching.Regex

object Day5 {

  val testData: LazyList[String] =
    """0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2""".stripMargin.splitAndTrimToList

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day5.txt").lines.filterNot(_.isEmpty)

  val VentRegex: Regex = "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)".r("x1", "y1", "x2", "y2")

  @main def d5p1(): Unit =
    def solve(seq: LazyList[String]): Unit =
      val vents: LazyList[Vent] = parse(seq)
      val counts = vents.filter(_.isGridAligned).flatMap(_.points).foldLeft(CountingMap[Point, Int]()) { case (map, point) => map.add(point)}
      val result = counts.map.filter(_._2 > 1).keys.size
      println(result)

    solve(testData)
    solve(data)

  @main def d5p2(): Unit =
    def solve(seq: LazyList[String]): Unit =
      val vents: LazyList[Vent] = parse(seq)
      val counts = vents.flatMap(_.points).foldLeft(CountingMap[Point, Int]()) { case (map, point) => map.add(point)}
      val result = counts.map.filter(_._2 > 1).keys.size
      println(result)

    solve(testData)
    solve(data)

  def parse(lines: LazyList[String]): LazyList[Vent] = lines.collect {
    case VentRegex(x1, y1, x2, y2) => Vent(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
  }

  case class Point(x: Int, y: Int)

  object Point {
    implicit val leftToRight: Ordering[Point] = Ordering.by(_.x)
  }

  case class Vent(start: Point, end: Point) {

    def isHorizontal: Boolean = start.y == end.y

    def isVertical: Boolean = start.x == end.x

    def isGridAligned: Boolean = isHorizontal || isVertical

    def points: Seq[Point] =
      if (isHorizontal) {
        val (min, max) = (start.x, end.x).minMax
        (min to max).map(x => Point(x, start.y))
      } else if (isVertical) {
        val (min, max) = (start.y, end.y).minMax
        (min to max).map(y => Point(start.x, y))
      } else {
        // Problem statement says these can only be at 45 degrees
        val (left, right) = (start, end).minMax
        val difference = right.x - left.x
        assert(difference > 0)
        val multiplier =
          if (right.y == left.y + difference) 1
          else if (right.y == left.y - difference) -1
          else throw new IllegalArgumentException("Not diagonal")

        (left.x to right.x).zipWithIndex.map { case (x, idx) => Point(x, left.y + idx * multiplier) }

      }
  }

}
