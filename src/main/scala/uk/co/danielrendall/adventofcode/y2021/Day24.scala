package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec
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
      val operations: List[Operation] =
        parse(seq)
          .grouped(18)
          .toList
          .map(instructions => InstructionGroup(instructions.toList))
          .map(_.toOperation)

      val ret = for {
        i3 <- 1 to 9
        z2 <- for {
          i2 <- 1 to 9
          z1 <- for {
            i1 <- 1 to 9
          } yield {
            operations(0).getNextZ(i1, 0)
          }
        } yield {
          operations(1).getNextZ(i2, z1)
        }
      } yield {
        operations(2).getNextZ(i3, z2)
      }
      ret.foreach(println)


    solve(data)


  // Note helpful
  @main def d24reverseEngineer() =
    def solve(seq: Seq[String]): Unit =
      val operations: List[Operation] =
        parse(seq)
          .grouped(18)
          .toList
          .map(instructions => InstructionGroup(instructions.toList))
          .map(_.toOperation)

      @tailrec
      def reverseEngineer(opsInReverseOrder: List[Operation],
                          targetAccum: List[Map[BigInt, List[InputAndRequiredZ]]]): List[Map[BigInt, List[InputAndRequiredZ]]] =
        println("Processing")
        opsInReverseOrder match {
          case op :: restOps =>

            targetAccum match {
              case map :: restTarget =>
                // Map is a map of (z output from previous stage -> combination of inputs and Zs which give that output)
                // We now need to work out what Zs and inputs to this stage will give us the Zs required at the following
                // stage (which, confusingly, we have just processed)
                val allZsToConsider = map.view.values.flatten.map(_.z).toList.sorted
                val nextMap = allZsToConsider.map(z => z -> investigate(op, z)).toMap
                reverseEngineer(restOps, nextMap :: targetAccum)
              case Nil =>
                val result: List[InputAndRequiredZ] = investigate(op, 0)
                reverseEngineer(restOps, List(Map(BigInt(0) -> result)))
            }
          case Nil => targetAccum.reverse
        }


      val result = reverseEngineer(operations.reverse, List.empty)

      println(result.toString())


    solve(data)


  /**
   * This demonstrates that the simplified version in "operation" executes the same algorithm (taking all
   */
  @main def d24verify() =
    def solve(seq: Seq[String]): Unit =
      val allInstructions = parse(seq)

      val groups: List[InstructionGroup] =
        allInstructions.grouped(18).toList.map(instructions => InstructionGroup(instructions.toList))

      val operations: List[Operation] = groups.map(_.toOperation)
      operations.foreach(println)

      val rnd = new Random()

      def r = rnd.nextInt(9) + 1

      (0 to 1000).foreach { i =>
        val state = State(0, 0, 0, 0, List(r, r, r, r, r, r, r, r, r, r, r, r, r, r))

        val finalState = (operations.zip(groups)).foldLeft(state) { case (thisState, (op, grp)) =>
          val newOpState = op.run(thisState).getOrElse(throw new Exception("Ops out of data"))
          val newGrpState = grp.run(thisState).getOrElse(throw new Exception("Groups out of data"))
          if (newOpState.cleaned != newGrpState.cleaned) {
            throw new Exception("Op state " + newOpState + " different from group state " + newGrpState)
          }
          newOpState
        }
        val finalState2 = runOnInput(state)
        if (finalState2.cleaned != finalState.cleaned) {
          throw new Exception("Final state 2 " + finalState2 + " different from final state " + finalState)
        }
        println(s"$state -> $finalState")
      }

    solve(data)

  @main def d24writeCode() =
    def solve(seq: Seq[String]): Unit =
      val allInstructions = parse(seq)

      val groups: List[InstructionGroup] =
        allInstructions.grouped(18).toList.map(instructions => InstructionGroup(instructions.toList))

      val operations: List[Operation] = groups.map(_.toOperation)

      val code: Seq[String] = operations.zipWithIndex.map { case (op, index) =>
        op.toScala(index)
      }
      code.foreach(println)


    solve(data)


  def investigate(operation: Operation, requiredZ: BigInt): List[InputAndRequiredZ] = {
    (1 to 9).flatMap { input =>
      (-1000000 to 1000000).flatMap { z =>
        operation.run(State(0, 0, 0, z, List(input))) match {
          case Some(newState) =>
            if (newState.z == requiredZ) {
              Some(InputAndRequiredZ(input, z))
            } else None
          case None =>
            None
        }
      }
    }.toList

  }

  def runOnInput(state: State): State = {

    val input = state.waitingInput
    val z0 = state.z

    val z1 = if (z0 % 26 + 10 - input(0) == 0) {
      z0 / 1
    } else {
      26 * (z0 / 1) + input(0) + 10
    }


    val z2 = if (z1 % 26 + 13 - input(1) == 0) {
      z1 / 1
    } else {
      26 * (z1 / 1) + input(1) + 5
    }


    val z3 = if (z2 % 26 + 15 - input(2) == 0) {
      z2 / 1
    } else {
      26 * (z2 / 1) + input(2) + 12
    }


    val z4 = if (z3 % 26 - 12 - input(3) == 0) {
      z3 / 26
    } else {
      26 * (z3 / 26) + input(3) + 12
    }


    val z5 = if (z4 % 26 + 14 - input(4) == 0) {
      z4 / 1
    } else {
      26 * (z4 / 1) + input(4) + 6
    }


    val z6 = if (z5 % 26 - 2 - input(5) == 0) {
      z5 / 26
    } else {
      26 * (z5 / 26) + input(5) + 4
    }


    val z7 = if (z6 % 26 + 13 - input(6) == 0) {
      z6 / 1
    } else {
      26 * (z6 / 1) + input(6) + 15
    }


    val z8 = if (z7 % 26 - 12 - input(7) == 0) {
      z7 / 26
    } else {
      26 * (z7 / 26) + input(7) + 3
    }


    val z9 = if (z8 % 26 + 15 - input(8) == 0) {
      z8 / 1
    } else {
      26 * (z8 / 1) + input(8) + 7
    }


    val z10 = if (z9 % 26 + 11 - input(9) == 0) {
      z9 / 1
    } else {
      26 * (z9 / 1) + input(9) + 11
    }


    val z11 = if (z10 % 26 - 3 - input(10) == 0) {
      z10 / 26
    } else {
      26 * (z10 / 26) + input(10) + 2
    }


    val z12 = if (z11 % 26 - 13 - input(11) == 0) {
      z11 / 26
    } else {
      26 * (z11 / 26) + input(11) + 12
    }


    val z13 = if (z12 % 26 - 12 - input(12) == 0) {
      z12 / 26
    } else {
      26 * (z12 / 26) + input(12) + 4
    }


    val z14 = if (z13 % 26 - 13 - input(13) == 0) {
      z13 / 26
    } else {
      26 * (z13 / 26) + input(13) + 11
    }

    State(0, 0, 0, z14, List.empty)

  }

  case class InputAndRequiredZ(input: Int, z: BigInt)


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
          val newZ = getNextZ(w, z)
          Some(State(0, 0, 0, newZ, rest))
        case Nil => None
      }
    }

    def getNextZ(w: BigInt, z: BigInt) = {
      if (z % 26 + v2 - w == 0) {
        z / v1
      } else {
        26 * (z / v1) + (w + v3)
      }
    }

    def toScala(index: Int): String = {
      val thisZ = "z" + (index + 1)
      val lastZ = "z" + index
      val input = "input(" + index + ")"
      s"""val $thisZ = if ($lastZ % 26 ${if (v2 >= 0) s"+ $v2" else s"$v2"} - $input == 0) {
         |  $lastZ / $v1
         |} else {
         |  26 * ($lastZ / $v1) + $input + $v3
         |}
         |
         |""".stripMargin

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
