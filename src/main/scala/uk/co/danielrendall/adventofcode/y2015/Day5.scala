package uk.co.danielrendall.adventofcode.y2015

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day5 {

  val testData1: LazyList[String] =
    """ugknbfddgicrmopn
      |aaa
      |jchzalrnumimnmhp
      |haegwjzuvuyypxyu
      |dvszwmarrgswjxmb""".stripMargin.splitAndTrimToList

  val testData2: LazyList[String] =
    """qjhvhtzxzqqjkmpb
      |xxyxx
      |uurcxstgmygtbstg
      |ieodomkazucvgmuy""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2015/day5.txt").lines

  val vowels = Set('a', 'e', 'i', 'o', 'u')

  @main def d5p1(): Unit = {
    def solve(list: LazyList[String]) =
      list.count(s => isNiceForPart1(s.toList, 0, ' ', false))

    println("Test: " + solve(testData1))
    println("Actual: " + solve(numbers))
  }

  @main def d5p2(): Unit = {
    def solve(list: LazyList[String]) =
      list.count(s => isNiceForPart2(s.toList, (' ', ' '), Set.empty, false, false))

    println("Test: " + solve(testData2))
    println("Actual: " + solve(numbers))
  }

  @tailrec
  def isNiceForPart1(chars: List[Char],
                     vowelCount: Int,
                     last: Char,
                     hasDouble: Boolean): Boolean =
    chars match
      case head :: rest =>
        if (head == 'b' && last == 'a') false
        else if (head == 'd' && last == 'c') false
        else if (head == 'q' && last == 'p') false
        else if (head == 'y' && last == 'x') false
        else isNiceForPart1(rest, vowelCount + (if (vowels.contains(head)) 1 else 0), head, hasDouble || head == last)
      case _ =>
        vowelCount >= 3 && hasDouble

  def isNiceForPart2(chars: List[Char],
                     lastPair:(Char, Char),
                     pairsMet: Set[(Char, Char)],
                     hasRepeatedPair: Boolean,
                     hasRepeatedLetter: Boolean): Boolean =
    chars match
      case head :: rest =>
        val newLastPair = (lastPair._2, head)
        isNiceForPart2(rest, newLastPair, pairsMet + lastPair, hasRepeatedPair || pairsMet.contains(newLastPair), hasRepeatedLetter || lastPair._1 == head)
      case _ =>
        hasRepeatedPair && hasRepeatedLetter



}

