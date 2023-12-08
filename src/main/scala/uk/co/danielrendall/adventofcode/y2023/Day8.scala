package uk.co.danielrendall.adventofcode.y2023

import uk.co.danielrendall.adventofcode.utils.NumberUtils.PrimeFactorSet
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day8 {

  val testData: LazyList[String] =
    """LLR
      |
      |AAA = (BBB, BBB)
      |BBB = (AAA, ZZZ)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin.splitAndTrimToList

  val testData2: LazyList[String] =
    """LR
      |
      |11A = (11B, XXX)
      |11B = (XXX, 11Z)
      |11Z = (11B, XXX)
      |22A = (22B, XXX)
      |22B = (22C, 22C)
      |22C = (22Z, 22Z)
      |22Z = (22B, 22B)
      |XXX = (XXX, XXX)""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2023/day8.txt").lines

  @main def d8p1(): Unit = {
    def solve(list: LazyList[String]): Int = {
      val data = parse(list)
      countSteps(data, "AAA", _ == "ZZZ")
    }


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d8p2(): Unit = {

    def solve(list: LazyList[String]) = {
      val data = parse(list)
      val result = data.transitions.keys.filter(_.endsWith("A")).toList.sorted.map(start => countSteps(data, start, _.endsWith("Z")))
      result.map(r => PrimeFactorSet((r))).reduce { case (pf1, pf2) => pf1.lcm(pf2)}.asBigInt
    }

    println("Test: " + solve(testData2))
    println("Actual: " + solve(numbers))
  }

  def parse(lines: Seq[String]): Data = {
    val list = lines.head
    val transitions = lines.drop(2).map { line =>
      val start = line.substring(0, 3)
      val left = line.substring(7, 10)
      val right = line.substring(12, 15)
      start -> (left, right)
    }.toMap
    Data(list, transitions)
  }

  def countSteps(data: Data, startNode: String, endTest: String => Boolean): Int = {

    @tailrec
    def step(remaining: List[Char], node: String, index: Int, looped: Int): Int =
      if (endTest(node)) {
        looped * data.list.length + index
      } else {
        remaining match
          case head :: rest =>
            val (left, right) = data.transitions.getOrElse(node, throw new Exception("No transitions for: " + node))
            if (head == 'L') {
              step(rest, left, index + 1, looped)
            } else if (head == 'R') {
              step(rest, right, index + 1, looped)
            } else {
              throw new Exception("Bad step: " + head)
            }

          case Nil =>
            step(data.list.toList, node, 0, looped + 1)
      }

    step(data.list.toList, startNode, 0, 0)
  }

  case class Data(list: String, transitions: Map[String, (String, String)])
}

