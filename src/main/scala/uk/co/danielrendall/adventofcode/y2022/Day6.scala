package uk.co.danielrendall.adventofcode.y2022

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day6 {

  val testData: LazyList[Char] =
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb".to(LazyList)

  val realData: LazyList[Char] = this.getClass.getResourceAsStream("/2022/day6.txt").lines.head.to(LazyList)

  @main def d6p1(): Unit =
    def solve(data: LazyList[Char]) =
      @tailrec
      def recurse(list: LazyList[Char], i: Int, c1: Char, c2: Char, c3: Char): Int = list.headOption match
        case Some(v) =>
          if (v != c1 && v != c2 && v != c3 && c1 != c2 && c1 != c3 && c2 != c3) i else recurse(list.tail, i+1, v, c1, c2)
        case None => -1
      recurse(data, 1, data.head, data.head, data.head)

    println("Test: " + solve(testData))
    println("Actual: " + solve(realData))

  @main def d6p2(): Unit =
    def solve(data: LazyList[Char]) =
      @tailrec
      def recurse(list: LazyList[Char], i: Int, last14: Array[Char]): Int = list.headOption match
        case Some(v) =>
          last14(i % 14) = v
          if (last14.toSet.size == 14) i else recurse(list.tail, i+1, last14)
        case None => -1
      recurse(data, 1, (s"${data.head}" * 14).toCharArray)

    println("Test: " + solve(testData))
    println("Actual: " + solve(realData))

}
