package uk.co.danielrendall.adventofcode.y2022

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.language.postfixOps
import scala.util.matching.Regex

object Day5 {

  val testData: LazyList[String] =
    """    [D]
      |[N] [C]
      |[Z] [M] [P]
      | 1   2   3
      |
      |move 1 from 2 to 1
      |move 3 from 1 to 3
      |move 2 from 2 to 1
      |move 1 from 1 to 2""".stripMargin.splitToList

  val input: LazyList[String] = this.getClass.getResourceAsStream("/2022/day5.txt").lines

  val Move: Regex = "move (\\d+) from (\\d+) to (\\d+)".r

  @main def d5p1(): Unit =
    def solve(list: LazyList[String]) =
      val grouped = list.groupSeparatedBy("")
      val state: Array[MutableStackState] = parseInitialPosition(grouped.head)
      grouped.tail.head.foreach {
        case Move(count, source, dest) =>
          val c = count.toInt
          // Arrays are 0-based
          val sourceState = state(source.toInt - 1)
          val destState = state(dest.toInt - 1)
          (0 until c).foreach { _ =>
            destState.push(sourceState.pop())
          }
      }
      state.map(_.peek()).mkString

    println("Test: " + solve(testData))
    println("Actual: " + solve(input))

  @main def d5p2(): Unit =
    def solve(list: LazyList[String]) =

      val grouped = list.groupSeparatedBy("")
      val state: Array[MutableStackState] = parseInitialPosition(grouped.head)

      grouped.tail.head.foreach {
        case Move(count, source, dest) =>
          val c = count.toInt
          // Arrays are 0-based
          val sourceState = state(source.toInt - 1)
          val destState = state(dest.toInt - 1)
          val popped = (0 until c).map(_ => sourceState.pop())
          popped.reverse.foreach(destState.push)
      }
      state.map(_.peek()).mkString

    println("Test: " + solve(testData))
    println("Actual: " + solve(input))

  def parseInitialPosition(lines: Seq[String]) =
    // Total number of lines minus bottom row which we don't care about
    val reversed = lines.reverse.toList
    val numberOfStacks = reversed.head.grouped(4).size
    val stackData: Array[Array[Char]] = reversed.tail.map(s => {
      (for {
        i <- 0 until numberOfStacks
        charIndex = i * 4 + 1
      } yield if (charIndex < s.length) s.charAt(charIndex) else ' ').toArray
    }).toArray
    val maxStackSize: Int = stackData.foldLeft(0)((acc, arr) => acc + arr.count(_ != ' '))

    val state: Array[MutableStackState] = MutableStackState(numberOfStacks, maxStackSize)

    stackData.foreach(_.zipWithIndex.foreach((c, i) => if (c != ' ') state(i).push(c) else {}))

    state

  class MutableStackState(arr: Array[Char]) {
    private var ptr: Int = -1

    def push(c: Char): Unit = {
      ptr = ptr + 1
      arr(ptr) = c
    }

    def pop(): Char = {
      val v = arr(ptr)
      ptr = ptr - 1
      v
    }

    def peek(): Char = arr(ptr)

    override def toString: String = "Stack: " + arr.slice(0, ptr + 1).mkString
  }


  object MutableStackState {
    def apply(numberOfStacks: Int, maxStackSize: Int): Array[MutableStackState] =
      (0 until numberOfStacks).map(_ => new MutableStackState(new Array[Char](maxStackSize))).toArray

  }
}
