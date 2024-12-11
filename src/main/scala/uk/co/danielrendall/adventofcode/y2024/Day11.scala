package uk.co.danielrendall.adventofcode.y2024

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.collection.mutable

object Day11 {

  val testData: LazyList[String] =
    """125 17""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2024/day11.txt").lines

  def evolve(line: String, count: Int): Map[BigInt, Long] = {
    val seq: Seq[BigInt] = line.split(' ').map(s => BigInt(s)).toSeq
    evolveMap(seq.groupBy(identity).map { case (b, s) => (b -> s.size.toLong) }, count)
  }

  // Rosie's approach; I was going to do something more complicated and painful
  def evolveMap(cur: Map[BigInt, Long], count: Int): Map[BigInt, Long] =
    if (count == 0) cur else {
      evolveMap(cur.toSeq
        .flatMap { case (num, count) => evolve(num).map(b => b -> count) }
        .groupBy(_._1)
        .map { case (b, seq) => b -> seq.map(_._2).sum }, count - 1)
    }

  def evolve(number: BigInt): Seq[BigInt] =
    lazy val str = number.toString()
    if (number == 0) Seq(BigInt(1))
    else if (str.length % 2 == 0) {
      Seq(BigInt(str.substring(0, str.length / 2)), BigInt(str.substring(str.length / 2)))
    } else Seq(number * 2024)

  @main def d11p1(): Unit = {
    def solve(list: LazyList[String]) =
      evolve(list.head, 25).values.sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d11p2(): Unit = {
    def solve(list: LazyList[String]) =
      evolve(list.head, 75).values.sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

}

