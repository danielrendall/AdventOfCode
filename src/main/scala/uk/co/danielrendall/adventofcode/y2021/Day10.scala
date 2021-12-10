package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day10 {

  val testData: LazyList[String] =
    """[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin.splitAndTrimToList

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day10.txt").lines.filterNot(_.isEmpty)

  @main def d10p1(): Unit =
    def solve(seq: LazyList[String]): Unit =
      val score = seq.map(parseLine).collect {
        case Left(charPair) => charPair
      }.map(_.score1).sum

      println(score)
    solve(testData)
    solve(data)

  @main def d10p2(): Unit =
    def solve(seq: LazyList[String]): Unit =
      val completions = seq.map(parseLine).collect {
        case Right(completion) if completion.nonEmpty => completion
      }
      val scoresSorted = completions.map( completion =>
        completion.foldLeft(BigInt(0)) { case (score, pair) => score * 5 + pair.score2 }).sorted

      val mid = scoresSorted((scoresSorted.length - 1)/2)
      println(mid)

    solve(testData)
    solve(data)


  def parseLine(line: String): Either[CharPair, List[CharPair]] =
    @tailrec
    def check(reminingInput: List[Char], stack: List[CharPair]): Either[CharPair, List[CharPair]]  =
      reminingInput match {
        case char :: restOfInput =>
          CharPair.openMap.get(char) match {
            case Some(opener) =>
              check(restOfInput, opener :: stack)
            case None =>
              CharPair.closeMap.get(char) match {
                case Some(actualCloser) =>
                  stack match {
                    case expectedCloser :: restOfStack if (expectedCloser == actualCloser) =>
                      check(restOfInput, restOfStack)
                    case _ =>
                      Left(actualCloser)
                  }
                case None =>
                  throw new IllegalStateException(s"Unexpected char '$char' in input")
              }
          }
        case _ => Right(stack)
      }
    check(line.toCharArray.toList, List.empty)




  sealed abstract class CharPair(val open: Char, val close: Char, val score1: Int, val score2: Int)

  object CharPair {
    lazy val all: Seq[CharPair] = Seq(PAREN, SQUARE, CURLY, ANGLE)

    val openMap: Map[Char, CharPair] = all.map(x => x.open -> x).toMap
    val closeMap: Map[Char, CharPair] = all.map(x => x.close -> x).toMap
  }
  case object PAREN extends CharPair('(', ')', 3, 1)
  case object SQUARE extends CharPair('[', ']', 57,2)
  case object CURLY extends CharPair('{', '}', 1197, 3)
  case object ANGLE extends CharPair('<', '>', 25137, 4)

}
