package uk.co.danielrendall.adventofcode.y2015

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.util.matching.Regex

object Day6 {

  val testData: LazyList[String] =
    """turn on 0,0 through 999,999
      |toggle 0,0 through 999,0
      |turn off 499,499 through 500,500""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2015/day6.txt").lines

  @main def d6p1(): Unit = {
    def solve(list: LazyList[String]) =
      val grid: Array[Boolean] = Array.fill(gridSide * gridSide)(false)
      list.map(Instruction.apply).foreach { instruction =>
        instruction.updateBool(grid)
      }
      grid.count(identity)

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d6p2(): Unit = {
    def solve(list: LazyList[String]) =
      val grid: Array[Int] = Array.fill(gridSide * gridSide)(0)
      list.map(Instruction.apply).foreach { instruction =>
        instruction.updateInt(grid)
      }
      grid.sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  val gridSide = 1000

  sealed abstract class OpType {
    def apply(b: Boolean): Boolean
    def apply(i: Int): Int
  }

  private case object TurnOn extends OpType {
    override def apply(b: Boolean): Boolean = true
    override def apply(i: Int): Int = i + 1
  }

  private case object TurnOff extends OpType{
    override def apply(b: Boolean): Boolean = false
    override def apply(i: Int): Int = if (i > 0) i - 1 else 0
  }

  private case object Toggle extends OpType{
    override def apply(b: Boolean): Boolean = !b
    override def apply(i: Int): Int = i + 2
  }
  case class Instruction(opType: OpType, xMin: Int, xMax: Int, yMin: Int, yMax: Int) {

    def updateBool(grid: Array[Boolean]): Unit = for {
      x <- (xMin until xMax)
      y <- (yMin until yMax)
      idx = y * gridSide + x
    } grid.update(idx, opType(grid(idx)))

    def updateInt(grid: Array[Int]): Unit = for {
      x <- (xMin until xMax)
      y <- (yMin until yMax)
      idx = y * gridSide + x
    } grid.update(idx, opType(grid(idx)))

  }

  object Instruction {

    private val instructionRegex: Regex = "^((?:turn on)|(?:turn off)|(?:toggle)) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)".r

    def apply(string: String): Instruction = string match
      case instructionRegex(op, x1, y1, x2, y2) => {
        val opType = if (op == "turn on") TurnOn else if (op == "turn off") TurnOff else if (op == "toggle") Toggle else throw IllegalArgumentException(op)
        val xMin = Math.min(x1.toInt, x2.toInt)
        val xMax = Math.max(x1.toInt, x2.toInt) + 1
        val yMin = Math.min(y1.toInt, y2.toInt)
        val yMax = Math.max(y1.toInt, y2.toInt) + 1
        Instruction(opType, xMin, xMax, yMin, yMax)
      }


  }

}

