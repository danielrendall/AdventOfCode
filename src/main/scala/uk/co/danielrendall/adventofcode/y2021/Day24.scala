package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.util.Random
import scala.util.matching.Regex

object Day24 {

  /**
   * Notes: data turns out to be a set of 252 instructions, but split into repeated groups of 18 instructions. Each
   * group of 18 starts with "inp w", and all the groups are identical except for 3 numbers which differ between them.
   *
   * Note that we can disregard the values of x, y and w after each group as they're always reset to specific values.
   * Only the value of z is carried forward.
   */

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day24.txt").lines

  @main def d24p1() =
    def solve(seq: Seq[String]): Unit =
      val allInstructions = parse(seq)

      val groups: List[InstructionGroup] =
        allInstructions.grouped(18).toList.map(instructions => InstructionGroup(instructions.toList))

      val operations: List[Operation] = groups.map(_.toOperation)
      operations.foreach(println)

      val rnd = new Random()

      def r = rnd.nextInt(9) + 1

      (0 to 10000).foreach { i =>
        val state = State(0, 0, 0, 0, List(r, r, r, r, r, r, r, r, r, r, r, r, r, r))

        val finalState = (operations.zip(groups)).foldLeft(state) { case (thisState, (op, grp)) =>
          val newOpState = op.run(thisState).getOrElse(throw new Exception("Ops out of data"))
          val newGrpState = grp.run(thisState).getOrElse(throw new Exception("Groups out of data"))
          if (newOpState.cleaned != newGrpState.cleaned) {
            throw new Exception("Op state " + newOpState + " different from group state " + newGrpState)
          }
          newOpState
        }
        println(s"$state -> $finalState")
      }

    solve(data)

  sealed trait CalculationStep {
    // Run attempts to consume an item from the state's input list and returns None if
    // there's nothing left
    def run(state: State): Option[State]
  }

  /**
   * Operation represents the operation carried out by each group of 18 instructions, and takes the 3 parameters.
   *
   * @param v1
   * @param v2
   * @param v3
   */
  case class Operation(v1: BigInt, v2: BigInt, v3: BigInt) extends CalculationStep {

    def run(state: State): Option[State] = {
      state.waitingInput match {
        case _ :: rest =>
          val w: BigInt = state.waitingInput.head
          val z: BigInt = state.z

          val newZ = if (z % 26 + v2 == w) {
            z / v1
          } else {
            26 * (z / v1) + (w + v3)
          }
          Some(State(0, 0, 0, newZ, rest))
        case Nil => None
      }
    }
  }

  case class InstructionGroup(instructions: List[Instruction]) extends CalculationStep {

    def run(initial: State): Option[State] =
      initial.waitingInput match {
        case _ :: _ =>
          // We know there's input, and the first instruction will consume it

          val nextState = instructions.foldLeft(initial) { case (state, next) =>
            next(state)
          }
          Some(nextState)
        case Nil => None
      }

    def toOperation: Operation = {
      // Just need the three parameters
      instructions match {
        case _ :: _ :: _ :: _ :: Div(Z, Lit(v1)) :: Add(X, Lit(v2)) :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: Add(Y, Lit(v3)) :: rest =>
          Operation(v1, v2, v3)
        case _ =>
          throw new IllegalArgumentException("Bad params")
      }
    }


  }


  sealed abstract class Reg(name: String) {
    override def toString: String = name

    def update(state: State, value: BigInt): State

    def get(state: State): BigInt
  }

  case class State(w: BigInt, x: BigInt, y: BigInt, z: BigInt, waitingInput: List[BigInt]) {
    override def toString: String = "W=%3d X=%3d Y=%3d Z=%3d Input=%s".format(w, x, y, z, waitingInput.map(_.toString).mkString)

    def cleaned: State = copy(w = 0, x = 0, y = 0)
  }

  object State {

    def apply(inputs: List[BigInt]): State = State(0, 0, 0, 0, inputs)

  }

  object Reg {
    def apply(s: String): Option[Reg] = s match {
      case "w" => Some(W)
      case "x" => Some(X)
      case "y" => Some(Y)
      case "z" => Some(Z)
      case _ => None
    }
  }

  case object W extends Reg("w") {
    override def update(state: State, value: BigInt): State =
      state.copy(w = value)

    override def get(state: State): BigInt =
      state.w
  }

  case object X extends Reg("x") {
    override def update(state: State, value: BigInt): State =
      state.copy(x = value)

    override def get(state: State): BigInt =
      state.x
  }

  case object Y extends Reg("y") {
    override def update(state: State, value: BigInt): State =
      state.copy(y = value)

    override def get(state: State): BigInt =
      state.y
  }

  case object Z extends Reg("z") {
    override def update(state: State, value: BigInt): State =
      state.copy(z = value)

    override def get(state: State): BigInt =
      state.z
  }

  case class Lit(num: BigInt) {
    override def toString: String = s"$num"
  }

  type RegOrLit = Reg | Lit

  object RegOrLit {
    def apply(s: String): RegOrLit = Reg(s).getOrElse(Lit(s.toInt))
  }

  sealed trait Instruction extends (State => State)

  object Instruction {
    def apply(op: String): (Reg, RegOrLit) => Instruction = op match {
      case "add" => Add.apply
      case "mul" => Mul.apply
      case "div" => Div.apply
      case "mod" => Mod.apply
      case "eql" => Eql.apply
      case _ => throw new Exception("Bad op: " + op)
    }
  }

  case class Inp(reg: Reg) extends Instruction {
    override def toString: String = s"inp $reg"

    def apply(state: State): State = state.waitingInput match {
      case h :: rest =>
        reg.update(state, h).copy(waitingInput = rest)
      case _ =>
        throw new Exception("No more input")
    }
  }

  sealed trait BinaryInstruction {
    self: Instruction =>
    def op1: Reg

    def op2: RegOrLit

    def apply(state: State): State = {
      val a = op1.get(state)
      val b = op2 match {
        case Lit(num) => num
        case r: Reg => r.get(state)
      }
      op1.update(state, run(a, b))
    }

    def run(a: BigInt, b: BigInt): BigInt
  }

  case class Add(op1: Reg, op2: RegOrLit) extends Instruction with BinaryInstruction {
    override def toString: String = s"add $op1 $op2"

    override def run(a: BigInt, b: BigInt): BigInt = a + b
  }

  case class Mul(op1: Reg, op2: RegOrLit) extends Instruction with BinaryInstruction {
    override def toString: String = s"mul $op1 $op2"

    override def run(a: BigInt, b: BigInt): BigInt = a * b
  }

  case class Div(op1: Reg, op2: RegOrLit) extends Instruction with BinaryInstruction {
    override def toString: String = s"div $op1 $op2"

    override def run(a: BigInt, b: BigInt): BigInt = a / b
  }

  case class Mod(op1: Reg, op2: RegOrLit) extends Instruction with BinaryInstruction {
    override def toString: String = s"mod $op1 $op2"

    override def run(a: BigInt, b: BigInt): BigInt = a % b
  }

  case class Eql(op1: Reg, op2: RegOrLit) extends Instruction with BinaryInstruction {
    override def toString: String = s"eql $op1 $op2"

    override def run(a: BigInt, b: BigInt): BigInt = if (a == b) 1 else 0
  }

  val InputRegex: Regex = "inp ([wxzy])".r("reg")
  val OperatorRegex: Regex = "(add|mul|div|mod|eql) ([wxzy]) (-?[0-9]+|[wxzy])".r("op", "first", "second")

  def parse(seq: Seq[String]): Seq[Instruction] = seq.map {
    case InputRegex(reg) =>
      Inp(Reg(reg).getOrElse(throw new Exception("Bad reg: " + reg)))
    case OperatorRegex(op, first, second) =>
      Instruction(op)(Reg(first).getOrElse(throw new Exception("Bad reg: " + first)), RegOrLit(second))
  }

}
