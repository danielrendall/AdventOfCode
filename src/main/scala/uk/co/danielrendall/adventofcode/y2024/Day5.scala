package uk.co.danielrendall.adventofcode.y2024

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.util.matching.Regex

object Day5 {

  val testData: LazyList[String] =
    """47|53
      |97|13
      |97|61
      |97|47
      |75|29
      |61|13
      |75|53
      |29|13
      |97|29
      |53|29
      |61|53
      |97|53
      |61|29
      |47|13
      |75|47
      |97|75
      |47|61
      |75|61
      |47|29
      |75|13
      |53|13
      |
      |75,47,61,53,29
      |97,61,53,29,13
      |75,29,13
      |75,97,47,61,53
      |61,13,29
      |97,13,75,29,47""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2024/day5.txt").lines

  @main def d5p1(): Unit = {
    def solve(list: LazyList[String]) =
      val pairs: Seq[Pair] = list.filter(_.contains("|")).collect {
        case PairRegex(first, second) => Pair(first.toInt, second.toInt)
      }
      val pages: LazyList[Seq[Int]] = list.filter(_.contains(",")).map(_.split(',').map(_.toInt).toSeq)
      implicit val pairOrdering: Ordering[Int] = PairOrdering(pairs)
      pages.filter(l => l.sorted == l).map(s => s((s.length - 1) / 2)).sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d5p2(): Unit = {

    def solve(list: LazyList[String]) =
      val pairs: Seq[Pair] = list.filter(_.contains("|")).collect {
        case PairRegex(first, second) => Pair(first.toInt, second.toInt)
      }
      val pages: LazyList[Seq[Int]] = list.filter(_.contains(",")).map(_.split(',').map(_.toInt).toSeq)
      implicit val pairOrdering: Ordering[Int] = PairOrdering(pairs)
      pages.filter(l => l.sorted != l).map(_.sorted).map(s => s((s.length - 1) / 2)).sum


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  val PairRegex: Regex = "([0-9]+)\\|([0-9]+)".r

  case class Pair(first: Int, second: Int)

  case class PairOrdering(pairs: Seq[Pair]) extends Ordering[Int] {

    val map: Map[Int, Set[Int]] = pairs.groupBy(_.first).map { case (first, pairs) => first -> pairs.map(_.second).toSet}.toMap

    override def compare(x: Int, y: Int): Int =
      if (x == y) {
        0
      } else {
        // map will not contain the "highest" value
        map.get(x) match
          case Some(value) if value.contains(y) => -1
          case _ => 1
      }
  }

}


