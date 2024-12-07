package uk.co.danielrendall.adventofcode.y2015

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object Day4 {

  val testData: LazyList[String] =
    """abcdef""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2015/day4.txt").lines

  val digest: MessageDigest = MessageDigest.getInstance("MD5")

  def md5(str: String): Array[Byte] =
    digest.reset()
    digest.digest(str.getBytes(StandardCharsets.UTF_8))

  @main def d4p1(): Unit = {
    def solve(list: LazyList[String]) =
      LazyList.from(1).find { i =>
        val bytes = md5(list.head + i)
        bytes(0) == 0 && bytes(1) == 0 && (bytes(2) >=0 && bytes(2) < 16)
      }.get

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d4p2(): Unit = {
    def solve(list: LazyList[String]) =
      LazyList.from(1).find { i =>
        val bytes = md5(list.head + i)
        bytes(0) == 0 && bytes(1) == 0 && bytes(2) == 0
      }.get

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }
}


