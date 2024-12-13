package uk.co.danielrendall.adventofcode.y2024

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.util.matching.Regex

object Day13 {

  val testData: LazyList[String] =
    """Button A: X+94, Y+34
      |Button B: X+22, Y+67
      |Prize: X=8400, Y=5400
      |
      |Button A: X+26, Y+66
      |Button B: X+67, Y+21
      |Prize: X=12748, Y=12176
      |
      |Button A: X+17, Y+86
      |Button B: X+84, Y+37
      |Prize: X=7870, Y=6450
      |
      |Button A: X+69, Y+23
      |Button B: X+27, Y+71
      |Prize: X=18641, Y=10279""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2024/day13.txt").lines

  def solvePuzzle(p: Puzzle): Option[Long] = {
    // mA + nB = C
    // m*a_x + n*b_x = c_x
    // m*a_y + n*b_y = c_y
    // m = (c_x - n*b_x) / a_x = (c_y - n*b_y) / a_y
    // (c_x - n*b_x) / a_x = (c_y - n*b_y) / a_y
    // (a_y*c_x - n*a_y*b_x) = (a_x*c_y - n*a_x*b_y)
    // (a_y*c_x - a_x*c_y) = n(a_y*b_x - a_x*b_y)
    // n = (a_y*c_x - a_x*c_y) / (a_y*b_x - a_x*b_y) = S / T
    val S = p.ay * p.cx - p.ax * p.cy
    val T = p.ay * p.bx - p.ax * p.by
    if (S % T == 0) {
      val n = S / T
      // m = (c_x - n*b_x) / a_x = U / V
      val U = p.cx - n * p.bx
      val V = p.ax
      if (U % V == 0) {
        val m = U / V
        Some(3 * m + n)
      } else {
        None
      }
    } else {
      None
    }
  }

  @main def d13p1(): Unit = {
    def solve(list: LazyList[String]) = {
      val puzzles = list.grouped(4).map(Puzzle.apply)
      puzzles.flatMap(solvePuzzle).sum
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d13p2(): Unit = {
    def solve(list: LazyList[String]) =
      val puzzles = list.grouped(4).map(Puzzle.apply).map(p => p.copy(cx=p.cx+10000000000000L, cy=p.cy+10000000000000L))
      puzzles.flatMap(solvePuzzle).sum


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  case class Puzzle(ax: Long, ay: Long, bx: Long, by: Long, cx: Long, cy: Long)

  object Puzzle {

    private val buttonRegex: Regex = "^Button [AB]: X\\+([0-9]+), Y\\+([0-9]+)$".r
    private val prizeRegex: Regex = "^Prize: X=([0-9]+), Y=([0-9]+)".r

    def apply(lines: Seq[String]): Puzzle = {
      assert(lines(0).startsWith("Button A: X"))
      assert(lines(1).startsWith("Button B: X"))
      assert(lines(2).startsWith("Prize: X="))

      val (ax, ay) = lines(0) match {
        case buttonRegex(x, y) => (x.toLong, y.toLong)
      }

      val (bx, by) = lines(1) match {
        case buttonRegex(x, y) => (x.toLong, y.toLong)
      }

      val (cx, cy) = lines(2) match {
        case prizeRegex(x, y) => (x.toLong, y.toLong)
      }

      Puzzle(ax, ay, bx, by, cx, cy)
    }

  }
}