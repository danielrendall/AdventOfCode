package uk.co.danielrendall.adventofcode.y2015

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day11 {

  val testData: LazyList[String] =
    """abcdefgh
      |ghijklmn""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2015/day11.txt").lines

  def isValid(password: String): Boolean = {
    lazy val hasForbidden = password.exists(c => c == 'i' || c == 'l' || c == 'o')
    lazy val zippedOnce = password.zip(password.tail)
    lazy val zippedTwice = zippedOnce.zip(zippedOnce.tail)
    lazy val firstPair = zippedOnce.indexWhere { case (a, b) => a == b }
    (!hasForbidden &&
      firstPair > -1 &&
      (zippedOnce.indexWhere { case (a, b) => a == b && a != zippedOnce(firstPair)._1 } > -1) &&
      zippedTwice.exists { case ((a, b), (c, d)) => b.toInt == (a + 1) && d.toInt == (b + 1) })
  }

  @tailrec
  def findNextValid(current: String): String =
    if (isValid(current)) current else {
      findNextValid(increment(current))
    }

  def increment(current: String): String =
    val chars = current.toCharArray
    def inc(at: Int): Unit =
      val c = chars(at)
      if (c < 'z') {
        chars(at) = (c + 1).toChar
      } else {
        chars(at) = 'a'
        inc(at - 1)
      }
    inc(current.length - 1)
    val forbidden = chars.indexWhere(c => c == 'i' || c == 'o' || c == 'l')
    if (forbidden > -1) {
      chars(forbidden) = (chars(forbidden) + 1).toChar
      (forbidden + 1 until current.length).foreach(idx => chars(idx) = 'a')
    }
    new String(chars)


  @main def d11p1(): Unit = {
    def solve(list: LazyList[String]) =
      list.map(findNextValid).toList

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d11p2(): Unit = {
    def solve(list: LazyList[String]) =
      list.map(findNextValid).map(s => findNextValid(increment(s))).toList

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }
}
