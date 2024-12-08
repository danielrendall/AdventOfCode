package uk.co.danielrendall.adventofcode.y2015

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day12 {

  val testData: LazyList[String] =
    """[1,2,3]
      |{"a":2,"b":4}
      |[[[3]]]
      |{"a":{"b":4},"c":-1}
      |{"a":[-1,1]}
      |[-1,{"a":1}]
      |[]
      |{}""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2015/day12.txt").lines

  // Horrible approach!
  def sumNumbers(remaining: List[Char], currentNum: String, accum: Int): Int =
    remaining match
      case head :: rest =>
        if (head.isDigit || head == '-') {
          if (head == '-' && currentNum.nonEmpty) {
            if (currentNum != "-") {
              sumNumbers(rest, "-", currentNum.toInt + accum)
            } else {
              sumNumbers(rest, "-", accum)
            }
          } else {
            sumNumbers(rest, s"${currentNum}$head", accum)
          }
        } else {
          if (currentNum.nonEmpty && currentNum != "-") {
            sumNumbers(rest, "", currentNum.toInt + accum)
          } else {
            sumNumbers(rest, "", accum)
          }
        }
      case Nil =>
        if (currentNum.nonEmpty && currentNum != "-") {
          currentNum.toInt + accum
        } else {
          accum
        }

  def computeSumPart1(value: JsValue): Int =
    value match
      case JsString(value) => 0
      case JsNumber(value) => value
      case JsArray(values) => values.map(computeSumPart1).sum
      case JsObj(map) => map.values.map(computeSumPart1).sum

  def computeSumPart2(value: JsValue): Int =
    value match
      case JsString(value) => 0
      case JsNumber(value) => value
      case JsArray(values) => values.map(computeSumPart2).sum
      case JsObj(map) => if (map.values.exists(_ == JsString("red"))) 0 else map.values.map(computeSumPart2).sum


  @main def dxp1(): Unit = {
    def solve(list: LazyList[String]) =
      list.map(parse).map(computeSumPart1).toList

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def dxp2(): Unit = {
    def solve(list: LazyList[String]) =
      list.map(parse).map(computeSumPart2).toList

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  sealed trait JsValue
  case class JsString(value: String) extends JsValue
  case class JsNumber(value: Int) extends JsValue
  case class JsArray(values: Seq[JsValue]) extends JsValue
  case class JsObj(map: Map[String, JsValue]) extends JsValue

  def parse(string: String): JsValue =
    new Parser(string.toCharArray).parseJsValue

  class Parser(source: Array[Char]) {

    var pos: Int = 0

    def parseJsValue: JsValue =
      peek match {
        case '{' => parseJsObj
        case '[' => parseJsArray
        case '\"' => parseJsString
        case _ => parseJsNumber
      }

    // There are no spaces
    private def parseJsObj: JsObj = {
      expect('{')
      @tailrec
      def readMap(accum: Map[String, JsValue]): JsObj = {
        peek match
          case '}' =>
            skip
            JsObj(accum)
          case '\"' =>
            val string = parseJsString.value
            expect(':')
            val obj = parseJsValue
            readMap(accum + (string -> obj))
          case ',' =>
            skip
            readMap(accum)
      }
      readMap(Map.empty)
    }

    private def parseJsArray: JsArray = {
      expect('[')
      @tailrec
      def readArray(accum: List[JsValue]): JsArray = {
        peek match
          case ']' =>
            skip
            JsArray(accum.reverse)
          case ',' =>
            skip
            readArray(accum)
          case _ =>
            readArray(parseJsValue :: accum)
      }
      readArray(List.empty)
    }

    private def parseJsString: JsString = {
      expect('"')
      @tailrec
      def readString(accum: String): JsString = {
        peek match
          case '"' =>
            skip
            JsString(accum)
          case x =>
            skip
            readString(accum + x)
      }
      readString("")
    }

    private def parseJsNumber: JsNumber = {
      @tailrec
      def readNumber(accum: String): JsNumber = {
        peek match
          case '-' =>
            if (accum.isEmpty) {
              skip
              readNumber("-")
            } else {
              throw new IllegalStateException(s"Found unexpected - at position ${pos}")
            }
          case x if x.isDigit =>
            skip
            readNumber(accum + x)
          case _ =>
            JsNumber(accum.toInt)
      }

      readNumber("")
    }


    private def expect(expected: Char): Unit =
      if (read != expected) throw new IllegalStateException(s"Expected '$expected' at position ${pos - 1}")

    private def read: Char = {
      val c = source(pos)
      pos = pos + 1
      c
    }

    private def peek: Char = {
      source(pos)
    }

    private def skip: Unit =
      pos = pos + 1
  }

}

