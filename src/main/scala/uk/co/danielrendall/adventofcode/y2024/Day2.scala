package uk.co.danielrendall.adventofcode.y2024

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day2 {

  val testData: LazyList[String] =
    """7 6 4 2 1
      |1 2 7 8 9
      |9 7 6 2 1
      |1 3 2 4 5
      |8 6 4 4 1
      |1 3 6 7 9""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2024/day2.txt").lines


  def isSafe(list: Seq[Int]): Boolean =
    val seq = list.zip(list.tail).map { case (a, b) => b - a }
    seq.forall(x => x >= 1 && x <= 3) || seq.forall(x => x >= -3 && x <= -1)

  def excise[T](seq: Seq[T], idx: Int): Seq[T] =
    if (idx == 0) seq.tail else {
      val (start, end) = seq.splitAt(idx)
      start.reverse.tail.reverse ++ end
    }

  @main def d2p1(): Unit = {
    def solve(list: LazyList[String]) =
      val reports: LazyList[Seq[Int]] = list.map(_.split(' ').map(_.toInt).toSeq)
      reports.count(isSafe)

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d2p2(): Unit = {

    def solve(list: LazyList[String]) =
      val reports: LazyList[Seq[Int]] = list.map(_.split(' ').map(_.toInt).toSeq)
      reports.count(l => isSafe(l) || (0 to l.length).exists(i => isSafe(excise(l, i))))

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }
}

