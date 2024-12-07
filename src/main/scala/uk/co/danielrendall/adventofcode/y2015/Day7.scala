package uk.co.danielrendall.adventofcode.y2015

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.collection.mutable
import scala.util.matching.Regex

object Day7 {

  val testData: LazyList[String] =
    """123 -> x
      |456 -> y
      |x AND y -> d
      |x OR y -> e
      |x LSHIFT 2 -> f
      |y RSHIFT 2 -> g
      |NOT x -> h
      |NOT y -> i""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2015/day7.txt").lines

  @main def d7p1(): Unit = {
    def solve(list: LazyList[String], which: String) =
      val operationSet = OperationSet(list.map(Operation.apply).toSet)
      operationSet.evaluate(which)

    println("Test: " + solve(testData, "d"))
    println("Actual: " + solve(numbers, "a"))
  }

  @main def d7p2(): Unit = {
    def solve(list: LazyList[String], which: String) =
      val initialOperationSet = OperationSet(list.map(Operation.apply).toSet)
      val initialA = initialOperationSet.evaluate(which)
      val newOperationSet = new OperationSet(initialOperationSet.operations.filterNot(op => op.isInstanceOf[Load] && op.out == "b") + Load(Value(initialA), "b"))
      newOperationSet.evaluate(which)

    println("Test: " + solve(testData, "e"))
    println("Actual: " + solve(numbers, "a"))
  }

  val mask: Int = 0x0000ffff
  
  sealed trait Operand

  object Operand {
    def apply(string: String): Operand = if (string.forall(_.isDigit)) Value(string.toInt) else Ref(string)
  }

  private case class Value(v: Int) extends Operand

  private case class Ref(r: String) extends Operand

  class OperationSet(val operations: Set[Operation]) {

    val orderedByOut: Map[String, Operation] = operations.map(op => op.out -> op).toMap

    def evaluate(out: String): Int = evaluateWithCache(out)(new mutable.HashMap[String, Int])


    private def evaluateWithCache(out: String)
                                 (implicit cache: mutable.HashMap[String, Int]): Int =

      def get(operand: Operand): Int = operand match
        case Value(v) => v
        case Ref(r) => evaluateWithCache(r)

      cache.getOrElseUpdate(out, orderedByOut.get(out) match {
        case Some(op) =>
          op match
            case Load(in, out) =>
              get(in)
            case And(in1, in2, out) =>
              (get(in1) & get(in2)) & mask
            case Or(in1, in2, out) =>
              (get(in1) | get(in2)) & mask
            case Not(in, out) =>
              ~get(in) & mask
            case LShift(in, amount, out) =>
              (get(in) << amount) & mask
            case RShift(in, amount, out) =>
              (get(in) >>> amount) & mask
        case None =>
          throw new IllegalStateException("Can't find operation which yields" + out)
      })

  }

  sealed trait Operation {
    def out: String
  }

  private case class Load(value: Operand, out: String) extends Operation

  private case class And(in1: Operand, in2: Operand, out: String) extends Operation

  private case class Or(in1: Operand, in2: Operand, out: String) extends Operation

  private case class Not(in: Operand, out: String) extends Operation

  private case class LShift(in: Operand, amount: Byte, out: String) extends Operation

  private case class RShift(in: Operand, amount: Byte, out: String) extends Operation

  object Operation {

    val load: Regex = "^([0-9a-z]+) -> ([a-z]+)$".r
    val and: Regex = "^([0-9a-z]+) AND ([0-9a-z]+) -> ([a-z]+)$".r
    val or: Regex = "^([0-9a-z]+) OR ([0-9a-z]+) -> ([a-z]+)$".r
    val not: Regex = "^NOT ([0-9a-z]+) -> ([a-z]+)$".r
    val lShift: Regex = "^([0-9a-z]+) LSHIFT ([0-9]+) -> ([a-z]+)$".r
    val rShift: Regex = "^([0-9a-z]+) RSHIFT ([0-9]+) -> ([a-z]+)$".r


    def apply(string: String): Operation = string match
      case load(a, b) => Load(Operand(a), b)
      case and(a, b, c) => And(Operand(a), Operand(b), c)
      case or(a, b, c) => Or(Operand(a), Operand(b), c)
      case not(a, b) => Not(Operand(a), b)
      case lShift(a, b, c) => LShift(Operand(a), b.toByte, c)
      case rShift(a, b, c) => RShift(Operand(a), b.toByte, c)

  }


}


