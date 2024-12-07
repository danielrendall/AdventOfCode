package uk.co.danielrendall.adventofcode.y2024

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day7 {

  val testData: LazyList[String] =
    """190: 10 19
      |3267: 81 40 27
      |83: 17 5
      |156: 15 6
      |7290: 6 8 6 15
      |161011: 16 10 13
      |192: 17 8 14
      |21037: 9 7 18 13
      |292: 11 6 16 20""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2024/day7.txt").lines

  @tailrec
  private def solvable(toTry: LazyList[Equation], operations: List[CombineOp]): Boolean =
    toTry.headOption match
      case Some(next) =>
        if (next.solved) {
          true
        } else {
          solvable(next.possibles(operations) ++ toTry.tail, operations)
        }
      case None =>
        false


  @main def d7p1(): Unit = {
    def solve(list: LazyList[String]) =
      val operations = List(multiplication, addition)
      list.map(Equation.apply)
        .filter(eq => solvable(eq.initial(operations), operations))
        .map(_.target)
        .sum


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d7p2(): Unit = {

    def solve(list: LazyList[String]) =
      val operations = List(multiplication, concatenation, addition)
      list.map(Equation.apply)
        .filter(eq => solvable(eq.initial(operations), operations))
        .map(_.target)
        .sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  case class Equation(target: Long, accum: Long, remaining: List[Long]) {

    def initial(operations: List[CombineOp]): LazyList[Equation] = remaining match {
      case first :: second :: rest =>
        (LazyList.unfold[Equation, List[CombineOp]](operations) {
          case op :: moreOps =>
            Option((copy(accum = op(first, second), remaining = rest)), moreOps)
          case _ => Option.empty
        }).filterNot(_.failed)
      case _ => throw new IllegalArgumentException("Not enough items to start")
    }

    def possibles(operations: List[CombineOp]): LazyList[Equation] = remaining match {
      case next :: rest =>
        (LazyList.unfold[Equation, List[CombineOp]](operations) {
          case op :: moreOps =>
            Option((copy(accum = op(accum, next), remaining = rest)), moreOps)
          case _ => Option.empty
        }).filterNot(_.failed)
      case _ => LazyList.empty
    }

    def solved: Boolean = accum == target && remaining.isEmpty

    def failed: Boolean = accum > target || (accum == target && remaining.exists(_ > 1))

  }

  private type CombineOp = (Long, Long) => Long

  private val addition: CombineOp = (l1: Long, l2: Long) => l1 + l2
  private val multiplication: CombineOp = (l1: Long, l2: Long) => l1 * l2
  private val concatenation: CombineOp = (l1: Long, l2: Long) => (l1.toString + l2.toString).toLong

  object Equation {

    def apply(line: String): Equation = {
      val bits = line.split(':')
      val target = bits(0).toLong
      val numbers = bits(1).trim.split(' ').map(_.trim.toLong).toList
      Equation(target, 0, numbers)
    }
  }
}

