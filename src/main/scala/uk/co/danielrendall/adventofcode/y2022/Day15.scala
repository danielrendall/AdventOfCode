package uk.co.danielrendall.adventofcode.y2022

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.util.matching.Regex

object Day15 {

  val testData: LazyList[String] =
    """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
      |Sensor at x=9, y=16: closest beacon is at x=10, y=16
      |Sensor at x=13, y=2: closest beacon is at x=15, y=3
      |Sensor at x=12, y=14: closest beacon is at x=10, y=16
      |Sensor at x=10, y=20: closest beacon is at x=10, y=16
      |Sensor at x=14, y=17: closest beacon is at x=10, y=16
      |Sensor at x=8, y=7: closest beacon is at x=2, y=10
      |Sensor at x=2, y=0: closest beacon is at x=2, y=10
      |Sensor at x=0, y=11: closest beacon is at x=2, y=10
      |Sensor at x=20, y=14: closest beacon is at x=25, y=17
      |Sensor at x=17, y=20: closest beacon is at x=21, y=22
      |Sensor at x=16, y=7: closest beacon is at x=15, y=3
      |Sensor at x=14, y=3: closest beacon is at x=15, y=3
      |Sensor at x=20, y=1: closest beacon is at x=15, y=3
      |""".stripMargin.splitAndTrimToList

  val games: LazyList[String] = this.getClass.getResourceAsStream("/2022/day15.txt").lines.filterNot(_.isEmpty)

  @main def d15p1(): Unit =
    def solve(list: LazyList[String], y: Int) =
      val sensorsAndBeacons: Seq[SensorAndBeacon] = list.map(parse)
      val spans = sensorsAndBeacons.flatMap(_.spanAtY(y))
      val beaconsOnRow = sensorsAndBeacons.filter(_.by == y).map(_.bx).distinct
      // Don't need to check if they're inside the spans; they have to be
      merge(spans).map(_.length).sum - beaconsOnRow.size

    println("Test: " + solve(testData, 10))
    println("Actual: " + solve(games, 2000000))

  @main def d15p2(): Unit =
    def solve(list: LazyList[String], xMin: Int, xMax: Int, yMin: Int, yMax: Int) =
      val sensorsAndBeacons: Seq[SensorAndBeacon] = list.map(parse)
      (yMin to yMax).to(LazyList).flatMap { y =>
        val merged = merge(sensorsAndBeacons.flatMap(_.spanAtY(y)))
        if (!merged.exists(s => s.start <= xMin && s.end >= xMax)) {
          if (merged.size == 2) {
            val x = merged.head.end + 1
            Some(x.toLong * 4000000L + y.toLong)
          } else None
        } else None
      }.head

    println("Test: " + solve(testData, 0, 20, 0, 20))
    println("Actual: " + solve(games, 0, 4000000, 0, 4000000))

  def merge(spans: Seq[Span]): Seq[Span] = {

    // accum always has a head
    def mergeSorted(toProcess: List[Span], accum: List[Span]): List[Span] = {
      toProcess match
        case head :: rest =>
          head.merge(accum.head) match
            case Some(merged) =>
              mergeSorted(rest, merged :: accum.tail)
            case None => mergeSorted(rest, head :: accum)

        case _ => accum.reverse
    }

    spans.toList.sortBy(_.start) match
      case head :: tail => mergeSorted(tail, List(head))
      case _ => Seq.empty

  }

  case class SensorAndBeacon(sx: Int, sy: Int, bx: Int, by: Int) {

    def distanceToBeacon = Math.abs(sx - bx) + math.abs(sy - by)

    def spanAtY(y: Int): Option[Span] = {
      val offset = Math.abs(y - sy)
      val extent = distanceToBeacon - offset
      if (extent >= 0) Some(Span(sx - extent, sx + extent)) else None
    }

    override def toString: String = s"Sensor: ($sx,$sy) Beacon: ($bx,$by) Distance: $distanceToBeacon"

  }

  private val LineRegex: Regex = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r

  private def parse(line: String) = line match
    case LineRegex(sx, sy, bx, by) => SensorAndBeacon(sx.toInt, sy.toInt, bx.toInt, by.toInt)
    case _ => throw new IllegalArgumentException("Bad line: " + line)

  // inclusive
  case class Span(start: Int, end: Int) {

    def otherStartsInsideOrJustAfter(other: Span): Boolean = other.start >= start && other.start <= end+1

    def otherEndsInsideOrJustBefore(other: Span): Boolean = other.end >= start - 1 && other.end <= end

    def otherIsCompletelyIncluded(other: Span): Boolean = other.start >= start && other.end <= end

    def length = end - start + 1

    def merge(other: Span): Option[Span] = {
      if (otherStartsInsideOrJustAfter(other)) {
        Some(Span(start, Math.max(end, other.end)))
      } else if (otherEndsInsideOrJustBefore(other)) {
        Some(Span(other.start, Math.max(other.end, end)))
      } else if (otherIsCompletelyIncluded(other)) {
        Some(this)
      } else if (other.otherIsCompletelyIncluded(this)) {
        Some(other)
      } else None

    }

  }
}
