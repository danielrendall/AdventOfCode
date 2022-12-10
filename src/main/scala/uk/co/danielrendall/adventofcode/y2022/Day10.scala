package uk.co.danielrendall.adventofcode.y2022

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day10 {

  val testData: LazyList[String] =
    """addx 15
      |addx -11
      |addx 6
      |addx -3
      |addx 5
      |addx -1
      |addx -8
      |addx 13
      |addx 4
      |noop
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx -35
      |addx 1
      |addx 24
      |addx -19
      |addx 1
      |addx 16
      |addx -11
      |noop
      |noop
      |addx 21
      |addx -15
      |noop
      |noop
      |addx -3
      |addx 9
      |addx 1
      |addx -3
      |addx 8
      |addx 1
      |addx 5
      |noop
      |noop
      |noop
      |noop
      |noop
      |addx -36
      |noop
      |addx 1
      |addx 7
      |noop
      |noop
      |noop
      |addx 2
      |addx 6
      |noop
      |noop
      |noop
      |noop
      |noop
      |addx 1
      |noop
      |noop
      |addx 7
      |addx 1
      |noop
      |addx -13
      |addx 13
      |addx 7
      |noop
      |addx 1
      |addx -33
      |noop
      |noop
      |noop
      |addx 2
      |noop
      |noop
      |noop
      |addx 8
      |noop
      |addx -1
      |addx 2
      |addx 1
      |noop
      |addx 17
      |addx -9
      |addx 1
      |addx 1
      |addx -3
      |addx 11
      |noop
      |noop
      |addx 1
      |noop
      |addx 1
      |noop
      |noop
      |addx -13
      |addx -19
      |addx 1
      |addx 3
      |addx 26
      |addx -30
      |addx 12
      |addx -1
      |addx 3
      |addx 1
      |noop
      |noop
      |noop
      |addx -9
      |addx 18
      |addx 1
      |addx 2
      |noop
      |noop
      |addx 9
      |noop
      |noop
      |noop
      |addx -1
      |addx 2
      |addx -37
      |addx 1
      |addx 3
      |noop
      |addx 15
      |addx -21
      |addx 22
      |addx -6
      |addx 1
      |noop
      |addx 2
      |addx 1
      |noop
      |addx -10
      |noop
      |noop
      |addx 20
      |addx 1
      |addx 2
      |addx 2
      |addx -6
      |addx -11
      |noop
      |noop
      |noop""".stripMargin.splitAndTrimToList

  val realData: LazyList[String] = this.getClass.getResourceAsStream("/2022/day10.txt").lines.filterNot(_.isEmpty)

  @main def d10p1(): Unit =
    def solve(list: LazyList[String]) =
      getXAndCycleNumber(list)
        .filter((x, c) => (c - 20) % 40 == 0)
        .takeWhile((x, c) => c <= 220)
        .map((x, c) => x * c).sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(realData))

  @main def d10p2(): Unit =
    def solve(list: LazyList[String]) =
      getXAndCycleNumber(list)
        .map { case (x, c) =>
          val cMod40 = ((c - 1) % 40)
          val spritePos = Set(x - 1, x, x + 1).filter(x => x >= 0 && x <= 39)
          if (spritePos.contains(cMod40)) '#' else '.'
        }.grouped(40)
        .map(_.mkString)
        .foreach(println)


    println("Test: " + solve(testData))
    println("Actual: " + solve(realData))

  def getXAndCycleNumber(list: LazyList[String]): LazyList[(Int, Int)] =
    LazyList.unfold((list.flatMap(parse), 1)) { case (ops, x) =>
      ops.headOption.map {
        case Noop => ((x, (ops.tail, x)))
        case Add(v) => ((x, (ops.tail, x + v)))
      }
      // zipWithIndex gives indexes starting at 0; we want indexes starting at 1
    }.zipWithIndex
      .map((x, c) => (x, c + 1))


  def parse(s: String): Seq[Op] =
    if (s == "noop") Seq(Noop)
    else if (s.startsWith("addx")) Seq(Noop, Add(s.substring(5).toInt))
    else throw new IllegalArgumentException("Bad instruction: " + s)

  sealed trait Op

  case object Noop extends Op

  case class Add(v: Int) extends Op
}
