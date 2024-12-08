package uk.co.danielrendall.adventofcode.y2015

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day10 {

  val testData: LazyList[String] =
    """1""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2015/day10.txt").lines

  def lookSay(value: String): String =
    @tailrec
    def recurse(remaining: List[Char], lastChar: Char, count: Int, accum: List[String]): String =
      remaining match
        case head :: rest =>
          if (head == lastChar) {
            recurse(rest, lastChar, count + 1, accum)
          } else {
            if (count > 0) {
              recurse(rest, head, 1, s"${count}${lastChar}" :: accum)
            } else {
              recurse(rest, head, 1, accum)
            }
          }
        case _ =>
          (s"${count}${lastChar}" :: accum).reverse.mkString
    recurse(value.toList, ' ', 0, List.empty)

  @main def d10p1(): Unit = {
    def solve(list: LazyList[String], iterations: Int) =
      val value = list.head
      (0 until iterations).foldLeft(value) { case (v, _) => lookSay(v)}.length

    println("Test: " + solve(testData, 5))
    println("Actual: " + solve(numbers, 40))
  }

  @main def d10p2(): Unit = {
    def solve(list: LazyList[String], iterations: Int) =
      val value = list.head
      (0 until iterations).foldLeft(value) { case (v, _) => lookSay(v)}.length

    println("Test: " + solve(testData, 5))
    println("Actual: " + solve(numbers, 50))
  }
}

