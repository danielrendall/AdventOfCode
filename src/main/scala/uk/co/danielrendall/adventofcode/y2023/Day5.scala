package uk.co.danielrendall.adventofcode.y2023

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec
import scala.collection.SortedSet

object Day5 {

  val testData: LazyList[String] =
    """seeds: 79 14 55 13
      |
      |seed-to-soil map:
      |50 98 2
      |52 50 48
      |
      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15
      |
      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4
      |
      |water-to-light map:
      |88 18 7
      |18 25 70
      |
      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13
      |
      |temperature-to-humidity map:
      |0 69 1
      |1 0 69
      |
      |humidity-to-location map:
      |60 56 37
      |56 93 4""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2023/day5.txt").lines

  def readInput(lines: List[String]): Data = {
    val seeds = lines.head.split(':')(1).trim.split(' ').map(_.toLong)

    @tailrec
    def readMap(remaining: List[String], maps: Map[String, RangeBasedMap]): Map[String, RangeBasedMap] = {
      remaining match
        case head :: rest =>
          val bits = head.split(' ').head.split('-')
          val from = bits(0)
          val to = bits(2)
          val numbers = rest.takeWhile(_.trim.nonEmpty)
          val ranges = numbers.map { s =>
            val innerBits = s.split(' ').map(_.trim)
            MapRange(innerBits(0).toLong, innerBits(1).toLong, innerBits(2).toLong)
          }.to(SortedSet)
          readMap(rest.drop(numbers.length + 1), maps + (from -> RangeBasedMap(from, to, ranges)))

        case _ =>
          maps
    }

    val maps = readMap(lines.drop(2), Map.empty)
    Data(seeds, maps)
  }

  @tailrec
  def find(startType: String, wantType: String, value: Long, data: Data): Long = {
    data.maps.get(startType) match {
      case Some(map) =>
        def mapped = map.map(value)
        if (map.to == wantType) {
          mapped
        } else {
          find(map.to, wantType, mapped, data)
        }

      case None =>
        throw new NoSuchElementException("No map starting with " + startType)
    }
  }

  @tailrec
  def findWithRanges(startType: String, wantType: String, currentRanges: List[Range], data: Data): List[Range] = {
    data.maps.get(startType) match {
      case Some(map) =>
        val newRanges = currentRanges.flatMap(r => map.mapRange(r))
        if (map.to == wantType) {
          newRanges
        } else {
          findWithRanges(map.to, wantType, newRanges, data)
        }

      case None =>
        throw new NoSuchElementException("No map starting with " + startType)
    }
  }

  @main def d5p1(): Unit = {
    def solve(list: LazyList[String]): Long = {
      val data = readInput(list.toList)
      data.seeds.map(v => find("seed", "location", v, data)).min
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d5p2(): Unit = {

    def solve(list: LazyList[String]): Long = {
      val data = readInput(list.toList)
      val finalRanges = findWithRanges("seed", "location", data.seedsAsRanges, data)
      finalRanges.map(_.start).min
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  case class Data(seeds: Seq[Long], maps: Map[String, RangeBasedMap]) {
    val seedsAsRanges: List[Range] = seeds.grouped(2).map(s => Range(s(0), s(1))).toList
  }

  case class RangeBasedMap(from: String, to: String, ranges: SortedSet[MapRange]) {
    def map(value: Long): Long = {
      find(ranges, value)._1
    }

    def mapRange(range: Range): List[Range] = {

      @tailrec
      def findMappingFor(currentRange: Range, accum: List[Range]): List[Range] = {
        if (currentRange.length > 0) {
          val (mapped, mapRangeOpt) = find(ranges, currentRange.start)
          mapRangeOpt match
            case Some(mapRange) =>
              // mapRange contains our current start, so we need to figure out whether the whole of our current range
              // is contained within this mapped range, or whether we need to split the range somehow
              val remainingInRange = mapRange.source + mapRange.length - currentRange.start
              if (currentRange.length < remainingInRange) {
                // Yes, the current range fits entirely in the mapped range, so going forward we have a new range
                // starting at the mapped value, with the same length
                (Range(mapped, currentRange.length) :: accum).reverse
              } else {
                // We have some overshoot...
                val newMappedRange = Range(mapped, remainingInRange)
                val newCurrentRange = Range(currentRange.start + remainingInRange, currentRange.length - remainingInRange)
                findMappingFor(newCurrentRange, newMappedRange :: accum)
              }
            case None =>
              // The start of the current range does not have a mapped value, so we need to figure out whether the whole
              // of the current range will be unmapped, or if at some point we will get a mapped value. So we need to
              // see which of the list of ranges (which is sorted) would come next
              ranges.find(_.source > currentRange.start) match
                case Some(nextRange) =>
                  val distanceToNextRange = nextRange.source - currentRange.start
                  if (distanceToNextRange >= currentRange.length) {
                    // We won't reach it
                    (currentRange :: accum).reverse
                  } else {
                    val newCurrentRange = Range(currentRange.start + distanceToNextRange, currentRange.length - distanceToNextRange)
                    findMappingFor(newCurrentRange, currentRange :: accum)
                  }
                case None =>
                  // None of the remaining things will be mapped, so we can just pass through the current range
                  assert(mapped == currentRange.start)
                  (currentRange :: accum).reverse
        } else accum.reverse
      }

      findMappingFor(range, List.empty)
    }

    @tailrec
    private def find(rem: SortedSet[MapRange], value: Long): (Long, Option[MapRange]) = rem.headOption match {
      case Some(head) =>
        head.map(value) match {
          case Some(result) =>
            (result, Some(head))
          case _ =>
            find(rem.tail, value)
        }
      case _ =>
        (value, None)
    }

  }

  case class MapRange(target: Long, source: Long, length: Long) {
    def map(value: Long): Option[Long] = {
      if (value >= source) {
        val diff = value - source
        if (diff < length) Some(target + diff) else None
      } else None
    }
  }

  object MapRange {
    implicit val ord: Ordering[MapRange] = Ordering.by[MapRange, Long](_.source)
  }

  case class Range(start: Long, length: Long)
}

