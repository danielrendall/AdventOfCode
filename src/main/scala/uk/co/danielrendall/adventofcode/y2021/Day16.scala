package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day16 {

  val data: String = this.getClass.getResourceAsStream("/2021/day16.txt").lines.filterNot(_.isEmpty).head

  private val hex2bin: Map[Char, String] = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  @main def d16p1() =
    def solve(string: String): Unit =
      implicit val binary: ListWrapper = ListWrapper(hexToBinary(string))
      val packet = readPacket(binary)
      println(packet)
      println(packet.versionSum)

    solve("D2FE28")
    solve("8A004A801A8002F478")
    solve("620080001611562C8802118E34")
    solve("C0015000016115A2E0802F182340")
    solve("A0016C880162017C3686B18A3D4780")
    solve(data)

  @main def d16p2() =
    def solve(string: String): Unit =
      implicit val binary: ListWrapper = ListWrapper(hexToBinary(string))
      val packet = readPacket(binary)
      println(packet)
      println(packet.finalValue)

    solve("C200B40A82")
    solve("04005AC33890")
    solve("880086C3E88112")
    solve("CE00C43D881120")
    solve("D8005AC2A8F0")
    solve("F600BC2D8F")
    solve("9C005AC2F8F0")
    solve("9C0141080250320F1802104A08")
    solve(data)


  def readPacket(implicit listWrapper: ListWrapper): Packet =
    val version: Read[BigInt] = listWrapper.read(3).map(_.parseBinary)
    val packetType: Read[BigInt] = listWrapper.read(3).map(_.parseBinary)
    packetType.value match {
      case 4 =>
        readLiteral(version, packetType)
      case n =>
        readOperator(version, packetType)
    }

  def readLiteral(version: Read[BigInt], packetType: Read[BigInt])
                 (implicit listWrapper: ListWrapper): LiteralPacket =
    @tailrec
    def readValue(charsRead: Int, accum: List[String]): LiteralPacket =
      val thisOne: Read[String] = listWrapper.read(5)
      thisOne.value.head match {
        case '0' =>
          // last group
          val value: BigInt = (thisOne.value.tail :: accum).reverse.mkString.parseBinary
          LiteralPacket(version.bitLength + packetType.bitLength + charsRead + 5, version.value, packetType.value, value)

        case '1' =>
          // more groups to come
          readValue(charsRead + 5, thisOne.value.tail :: accum)
      }
    readValue(0, List.empty)


  def readOperator(version: Read[BigInt], packetType: Read[BigInt])
                  (implicit listWrapper: ListWrapper): OperatorPacket =
    val lengthType: Read[String] = listWrapper.read(1)

    lengthType.value match {
      case "0" =>
        val bitLength: Read[BigInt] = listWrapper.read(15).map(_.parseBinary)
        readPacketsForNBits(version, packetType, lengthType, bitLength)
      case "1" =>
        val numberOfPackets: Read[BigInt] = listWrapper.read(11).map(_.parseBinary)
        readNPackets(version, packetType, lengthType, numberOfPackets)

      case _ =>
        throw new Exception("Bad length type " + lengthType.value)
    }

  def readPacketsForNBits(version: Read[BigInt], packetType: Read[BigInt], lengthType: Read[String], bitLength: Read[BigInt])
                         (implicit listWrapper: ListWrapper): OperatorPacket =
//    println(s"Reading ${bitLength.value} bits")
    @tailrec
    def read(bitsToRead: BigInt, accum: List[Packet]): OperatorPacket =
//      println(s"Got $bitsToRead of ${bitLength.value} bits to read")
      if (bitsToRead < 0) {
        throw new Exception("Read too many bits!")
      } else if (bitsToRead == 0) {
        OperatorPacket(version.bitLength + packetType.bitLength + lengthType.bitLength + 15 + accum.map(_.bitLength).sum, version.value, packetType.value, accum.reverse)
      } else {
        val next = readPacket
        read(bitsToRead - next.bitLength, next :: accum)
      }
    read(bitLength.value, List.empty)

  def readNPackets(version: Read[BigInt], packetType: Read[BigInt], lengthType: Read[String], numberOfPackets: Read[BigInt])
                         (implicit listWrapper: ListWrapper): OperatorPacket =
//    println(s"Reading ${numberOfPackets.value} packets")
    @tailrec
    def read(packetsRemaining: BigInt, accum: List[Packet]): OperatorPacket =
//      println(s"Got $packetsRemaining of ${numberOfPackets.value} packets to read")
      if (packetsRemaining < 0) {
        throw new Exception("Read too many packets!")
      } else if (packetsRemaining == 0) {
        OperatorPacket(version.bitLength + packetType.bitLength + lengthType.bitLength + 11 + accum.map(_.bitLength).sum, version.value, packetType.value, accum.reverse)
      } else {
        val next = readPacket
        read(packetsRemaining - 1, next :: accum)
      }
    read(numberOfPackets.value, List.empty)




  // Mutable
  class ListWrapper(initial: LazyList[Char]) {
    var current: LazyList[Char] = initial

    def read(bits: BigInt): Read[String] =
      @tailrec
      def doRead(count: BigInt, accum: List[Char]): Read[String] =
        if (count == 0) {
          Read(bits, accum.reverse.mkString)
        } else {
          current.headOption match {
            case Some(next) =>
              current = current.tail
              doRead(count - 1, next :: accum)
            case _ => throw new Exception("Ran out of elements trying to read " + bits)
          }
        }
      doRead(bits, List.empty)
  }

  object ListWrapper {
    def apply(initial: LazyList[Char]) = new ListWrapper(initial)
  }


  def hexToBinary(str: String): LazyList[Char] =
    str.to(LazyList).flatMap(c => hex2bin(c))

  sealed trait Packet {
    def bitLength: BigInt
    def version: BigInt
    def packetType: BigInt
    def versionSum: BigInt
    def finalValue: BigInt
  }

  case class LiteralPacket(bitLength: BigInt, version: BigInt, packetType: BigInt, value: BigInt) extends Packet {
    assert(packetType == 4, "Not a literal packet")
    override def toString: String = s"LITERAL v$version ($value)"
    def versionSum: BigInt = version
    def finalValue: BigInt = value
  }

  case class OperatorPacket(bitLength: BigInt, version: BigInt, packetType: BigInt, contents: Seq[Packet]) extends Packet {
    override def toString: String = s"OPERATOR v$version t$packetType {${contents.mkString(", ")}"
    def versionSum: BigInt = version + contents.map(_.versionSum).sum

    override def finalValue: BigInt = packetType.toInt match {
      case 0 => // sum
        contents.map(_.finalValue).sum
      case 1 => // product
        contents.map(_.finalValue).product
      case 2 => // min
        contents.map(_.finalValue).min
      case 3 => // max
        contents.map(_.finalValue).max
      case 5 => // gt
        if (contents(0).finalValue > contents(1).finalValue) 1 else 0

      case 6 => // lt
        if (contents(0).finalValue < contents(1).finalValue) 1 else 0

      case 7 => // eq
        if (contents(0).finalValue == contents(1).finalValue) 1 else 0
    }
  }

  case class Read[+T](bitLength: BigInt, value: T) {
    def map[W](fn: T => W): Read[W] = Read(bitLength, fn(value))
  }

  object Read {
    def seq[T](reads: Seq[Read[T]]): Read[Seq[T]] =
      Read(reads.map(_.bitLength).sum, reads.map(_.value))
  }

  extension (string: String)
    def parseBinary: BigInt = string.reverse.zipWithIndex.foldLeft((BigInt(0), BigInt(1))) { case ((cur, pow), (c, idx)) =>
      if (c == '0') (cur, pow * 2) else (cur + pow, pow * 2)
    }._1
}
