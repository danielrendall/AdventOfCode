import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*

import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import math.Ordered.orderingToOrdered

object Day13 {

  val testData: LazyList[String] =
    """[1,1,3,1,1]
      |[1,1,5,1,1]
      |
      |[[1],[2,3,4]]
      |[[1],4]
      |
      |[9]
      |[[8,7,6]]
      |
      |[[4,4],4,4]
      |[[4,4],4,4,4]
      |
      |[7,7,7,7]
      |[7,7,7]
      |
      |[]
      |[3]
      |
      |[[[]]]
      |[[]]
      |
      |[1,[2,[3,[4,[5,6,7]]]],8,9]
      |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin.splitAndTrimToList

  val realData: LazyList[String] = this.getClass.getResourceAsStream("/2022/day13.txt").lines.map(_.trim)

  @main def d13p1(): Unit =
    def solve(list: LazyList[String]) =
      val stringPairs = list.groupSeparatedBy("").map(s => (s(0), s(1)))
      stringPairs.flatMap { case (s1, s2) =>
        (for {
          packet1 <- parseListPacket(s1)
          packet2 <- parseListPacket(s2)
        } yield {
          packet1.t compare packet2.t
        }).toOption
      }.zipWithIndex.filter(_._1 < 0).map(_._2 + 1).sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(realData))

  @main def d13p2(): Unit =
    def solve(list: LazyList[String]) =
      val dividers = List("[[2]]", "[[6]]")
      val dividerPackets = dividers.map(parseListPacket).map(_.get.t)
      val listPackets = (list.filterNot(_.isEmpty) ++ dividers).map(parseListPacket).map(_.get.t)
      listPackets.sorted.zipWithIndex.filter(p => dividerPackets.contains(p._1)).map(_._2+1).product

    println("Test: " + solve(testData))
    println("Actual: " + solve(realData))


  sealed trait Packet

  object Packet {
    implicit object PacketOrdering extends Ordering[Packet] {
      override def compare(left: Packet, right: Packet): Int =
        (left, right) match
          case (IntPacket(l), IntPacket(r)) => l compare r
          case (ListPacket(l), ListPacket(r)) =>
            l.map(Option.apply).zipAll(r.map(Option.apply), Option.empty, Option.empty).map {
              case (Some(leftPacket), Some(rightPacket)) => leftPacket compare rightPacket
              case (Some(_), None) => 1
              case (None, Some(_)) => -1
              case (None, None) => throw new IllegalStateException("Shouldn't have had two Nones after zipAll")
            }.find(_ != 0).getOrElse(0)
          case (l@IntPacket(_), r@ListPacket(_)) => ListPacket(List(l)) compare r
          case (l@ListPacket(_), r@IntPacket(_)) => l compare ListPacket(List(r))

    }
  }

  case class ListPacket(children: List[Packet]) extends Packet

  case class IntPacket(value: Int) extends Packet

  case class ParseResult[T](t: T, rest: String)


  type Parser[T] = String => Try[ParseResult[T]]

  def parseSpecific(expected: Char)(string: String): Try[ParseResult[Char]] =
    if (string.headOption.contains(expected))
      Success(ParseResult(expected, string.tail))
    else
      Failure(new IllegalArgumentException("Expected " + expected + " but string was " + string))

  def alternatives[T](parsers: Parser[T]*): Parser[T] = input => {
    parsers.map(_(input)).collectFirst {
      case s@Success(_) => s
    }.getOrElse(Failure(new IllegalArgumentException("No parser matched " + input)))
  }

  def zeroOrMore[T](parser: Parser[T]): Parser[List[T]] = input => {
    @tailrec
    def tryParse(str: String, accum: List[T]): Try[ParseResult[List[T]]] =
      parser.apply(str) match
        case Failure(exception) =>
          Success(ParseResult(accum.reverse, str))
        case Success(value) =>
          tryParse(value.rest, value.t :: accum)

    tryParse(input, List.empty)
  }

  def parseSequential[T,U](parserT: Parser[T], parserU: Parser[U]): Parser[(T, U)] = input =>
    for {
      resT <- parserT(input)
      resU <- parserU(resT.rest)
    } yield ParseResult((resT.t, resU.t), resU.rest)

  lazy val LEFT_BRACKET: String => Try[ParseResult[Char]] = parseSpecific('[')
  lazy val COMMA: String => Try[ParseResult[Char]] = parseSpecific(',')
  lazy val RIGHT_BRACKET: String => Try[ParseResult[Char]] = parseSpecific(']')

  lazy val parseEmptyListPacket: Parser[Packet] = input =>
    for {
      start <- LEFT_BRACKET(input)
      end <- RIGHT_BRACKET(start.rest)
    } yield {

      ParseResult(ListPacket(List.empty), end.rest)
    }


  lazy val parseNonEmptyListPacket: Parser[Packet] = input =>
    for {
      start <- LEFT_BRACKET(input)
      item1 <- parsePacket(start.rest)
      moreItems <- zeroOrMore(parseCommaThenPacket)(item1.rest)
      end <- RIGHT_BRACKET(moreItems.rest)
    } yield {

      ParseResult(ListPacket(item1.t :: moreItems.t.map(_._2)), end.rest)
    }

  lazy val parseIntPacket: Parser[Packet] = input => Try {
    val digits = input.takeWhile(Character.isDigit)
    ParseResult(IntPacket(digits.toInt), input.drop(digits.length))
  }

  lazy val parseListPacket: Parser[Packet] = alternatives(parseNonEmptyListPacket, parseEmptyListPacket)

  lazy val parsePacket: Parser[Packet] = alternatives(parseListPacket, parseIntPacket)

  lazy val parseCommaThenPacket: Parser[(Char, Packet)] = parseSequential(COMMA, parsePacket)


}
