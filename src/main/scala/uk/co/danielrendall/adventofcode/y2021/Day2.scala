package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day2 {

  val testData: LazyList[String] =
    """forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2""".stripMargin.splitAndTrimToList

  val instructions: LazyList[Instruction] =
    this.getClass.getResourceAsStream("/2021/day2.txt").lines.filterNot(_.isEmpty).map(Instruction.fromString)

  @main def d2p1(): Unit =
    val finalPos = instructions.foldLeft(Position.start) { case (pos, i) => i.update(pos) }
    println(finalPos)
    println(finalPos.h * finalPos.d)

  @main def d2p2(): Unit =
    val finalPos = instructions.foldLeft(PositionWithAim.start) { case (pos, i) => i.update(pos) }
    println(finalPos)
    println(finalPos.h * finalPos.d)


  sealed trait Instruction {
    def update(position: Position): Position

    def update(positionWithAim: PositionWithAim): PositionWithAim
  }

  object Instruction {

    def fromString(s: String): Instruction =
      val bits = s.split(" ")
      if (bits.size == 2) {
        val units = bits(1).toInt
        bits(0) match {
          case "forward" => Forward(units)
          case "down" => Down(units)
          case "up" => Up(units)
          case x => throw new IllegalArgumentException("Bad direction: " + x)
        }
      } else {
        throw new IllegalArgumentException("Bad instruction: " + s)
      }

  }

  case class Forward(units: Int) extends Instruction {
    override def update(p: Position): Position =
      Position(p.h + units, p.d)

    override def update(p: PositionWithAim): PositionWithAim =
      PositionWithAim(p.h + units, p.d + (p.aim * units), p.aim)
  }

  case class Down(units: Int) extends Instruction {
    override def update(p: Position): Position =
      Position(p.h, p.d + units)

    override def update(p: PositionWithAim): PositionWithAim =
      PositionWithAim(p.h, p.d, p.aim + units)
  }

  case class Up(units: Int) extends Instruction {
    override def update(p: Position): Position =
      Position(p.h, p.d - units)

    override def update(p: PositionWithAim): PositionWithAim =
      PositionWithAim(p.h, p.d, p.aim - units)
  }

  case class Position(h: Int, d: Int)

  object Position {
    val start: Position = Position(0, 0)
  }

  case class PositionWithAim(h: Int, d: Int, aim: Int)

  object PositionWithAim {
    val start: PositionWithAim = PositionWithAim(0, 0, 0)
  }
}
