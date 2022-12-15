
import uk.co.danielrendall.adventofcode.utils.ArrayUtils
import uk.co.danielrendall.adventofcode.utils.ArrayUtils.{Array2D, Loc}
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable

object Day14 {

  val testData: LazyList[String] =
    """498,4 -> 498,6 -> 496,6
      |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin.splitAndTrimToList

  val games: LazyList[String] = this.getClass.getResourceAsStream("/2022/day14.txt").lines.filterNot(_.isEmpty)

  @main def d14p1(): Unit =
    def solve(list: LazyList[String]) =
      val runs = list.flatMap(parseIntoRuns)
      val xMin = runs.map(_.xMin).min
      val xMax = runs.map(_.xMax).max
      val yMin = runs.map(_.yMin).min
      val yMax = runs.map(_.yMax).max

      val arr: Array2D[Char] = Array2D.fill(xMax + 1, yMax + 1)('.')
      runs.foreach(_.update(arr))
      simulateSand(arr)



    println("Test: " + solve(testData))
    println("Actual: " + solve(games))

  @main def d14p2(): Unit =
    def solve(list: LazyList[String]) =
      val runs = list.flatMap(parseIntoRuns)
      val xMin = runs.map(_.xMin).min
      val xMax = runs.map(_.xMax).max
      val yMin = runs.map(_.yMin).min
      val yMax = runs.map(_.yMax).max

      val arr: Array2D[Char] = Array2D.fill(1002, yMax + 2)('.')
      runs.foreach(_.update(arr))
      // Way too big, never mind...
      (1 to 1002).foreach(x => arr.set(Loc(x, yMax + 2),'#'))
      // Off-by-one error - misses final grain
      val ret = simulateSand(arr) + 1
      ret

    println("Test: " + solve(testData))
    println("Actual: " + solve(games))

  def simulateSand(arr: Array2D[Char]): Int = {

    val startLoc = Loc(501, 1)

    @tailrec
    def sim(sandLoc: Loc): Boolean = {
      if (sandLoc.down.y > arr.height) false
      else {
        val toTry = Seq(sandLoc.down, sandLoc.downLeft, sandLoc.downRight)
        toTry.find(l => arr.get(l) == '.') match
          case Some(value) =>
            sim(value)
          case None =>
            arr.set(sandLoc, 'o')
            sandLoc != startLoc
      }
    }

    @tailrec
    def run(grains: Int): Int =
      if (sim(startLoc))
        run(grains + 1)
      else
        grains

    run(0)
  }

  sealed trait Run {
    def xMin: Int
    def xMax: Int
    def yMin: Int
    def yMax: Int
    def update(arr: Array2D[Char]): Unit
  }
  case class Horiz(left: Loc, right: Loc) extends Run {
    override def xMin: Int = left.x

    override def xMax: Int = right.x

    override def yMin: Int = left.y

    override def yMax: Int = left.y

    override def update(arr: Array2D[Char]): Unit = (xMin to xMax).foreach(x => arr.set(Loc(x, left.y), '#'))
  }
  case class Vert(top: Loc, bottom: Loc) extends Run {
    override def xMin: Int = top.x

    override def xMax: Int = top.x

    override def yMin: Int = top.y

    override def yMax: Int = bottom.y

    override def update(arr: Array2D[Char]): Unit = (yMin to yMax).foreach(y => arr.set(Loc(top.x, y), '#'))
  }

  val RunRegex= "^(\\d+),(\\d+) -> (\\d+),(\\d+)(.*)".r

  def parseIntoRuns(line: String): Seq[Run] = {

    @tailrec
    def parse(s: String, accum: List[Run]): List[Run] =
      s match {
        case RunRegex(p1, p2, p3, p4, rem) =>
          // Offset everything by 1 since their first row is row 0
          val d1 = p1.toInt + 1
          val d2 = p2.toInt + 1
          val d3 = p3.toInt + 1
          val d4 = p4.toInt + 1
          val rest = s"$p3,$p4$rem"
          val run: Run = if (d1 == d3 && d2 != d4) {
            if (d2 < d4) {
              Vert(Loc(d1, d2), Loc(d3, d4))
            } else {
              Vert(Loc(d3, d4), Loc(d1, d2))
            }
          } else if (d2 == d4 && d1 != d2) {
            if (d1 < d3) {
              Horiz(Loc(d1, d2), Loc(d3, d4))
            } else {
              Horiz(Loc(d3, d4), Loc(d1, d2))
            }
          } else {
            throw new IllegalArgumentException("Bad run: " + s)
          }
          parse(rest, run :: accum)

        case _ => accum
      }

    parse(line, List.empty)
  }
}

