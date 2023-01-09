package uk.co.danielrendall.adventofcode.y2022

import uk.co.danielrendall.adventofcode.utils.ArrayUtils.Loc
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.FifoQueue
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.annotation.tailrec
import scala.util.Try
import scala.util.matching.Regex

object Day11 {

  val testData: LazyList[String] =
    """Monkey 0:
      |  Starting items: 79, 98
      |  Operation: new = old * 19
      |  Test: divisible by 23
      |    If true: throw to monkey 2
      |    If false: throw to monkey 3
      |
      |Monkey 1:
      |  Starting items: 54, 65, 75, 74
      |  Operation: new = old + 6
      |  Test: divisible by 19
      |    If true: throw to monkey 2
      |    If false: throw to monkey 0
      |
      |Monkey 2:
      |  Starting items: 79, 60, 97
      |  Operation: new = old * old
      |  Test: divisible by 13
      |    If true: throw to monkey 1
      |    If false: throw to monkey 3
      |
      |Monkey 3:
      |  Starting items: 74
      |  Operation: new = old + 3
      |  Test: divisible by 17
      |    If true: throw to monkey 0
      |    If false: throw to monkey 1""".stripMargin.splitAndTrimToList.filterNot(_.isEmpty)

  val realData: LazyList[String] = this.getClass.getResourceAsStream("/2022/day11.txt").lines.map(_.trim).filterNot(_.isEmpty)

  @main def d11p1(): Unit =
    def solve(list: LazyList[String]) =
      val allMonkeys = parseMonkeys(list, true).toArray
      val modulo = allMonkeys.map(_.test).product
      (1 to 20).foreach { _ =>
        allMonkeys.foreach(_.processQueue(allMonkeys, modulo))
      }
      allMonkeys.map(_.handled).sorted(Ordering.Long.reverse).take(2).product


    println("Test: " + solve(testData))
    println("Actual: " + solve(realData))

  @main def d11p2(): Unit =
    def solve(list: LazyList[String]) =
      val allMonkeys = parseMonkeys(list, false).toArray
      val modulo = allMonkeys.map(_.test).product
      (1 to 10000).foreach { r =>
        allMonkeys.foreach(_.processQueue(allMonkeys, modulo))
      }
      allMonkeys.map(_.handled).sorted(Ordering.Long.reverse).take(2).product

    println("Test: " + solve(testData))
    println("Actual: " + solve(realData))


  def parseMonkeys(lines: Seq[String], divideByThree: Boolean): List[Monkey] = {

    @tailrec
    def parse(remaining: Seq[String], accum: List[Monkey]): List[Monkey] = {
      if (remaining.nonEmpty) {
        val m = parseMonkey(remaining, divideByThree).get
        parse(m.rest, m.result :: accum)
      } else accum.reverse
    }

    parse(lines, List.empty)
  }

  def parseMonkey(lines: Seq[String], divideByThree: Boolean): Try[ParseResult[String, Monkey]] =
    for {
      mh <- lines.parse(monkeyHeader)
      si <- mh.parse(s => parseFrom(s, "Starting items: ").map(_.split(',').map(_.trim.toLong).toSeq))
      op <- si.parse(s => parseFrom(s, "Operation: new = old ").flatMap(parseOperation))
      ts <- op.parse(s => parseFrom(s, "Test: divisible by ").map(_.toLong))
      to <- ts.parse(s => parseFrom(s, "If true: throw to monkey ").map(_.toInt))
      fl <- to.parse(s => parseFrom(s, "If false: throw to monkey ").map(_.toInt))
    } yield ParseResult(new Monkey(mh(), op(), ts(), to(), fl(), divideByThree, new MonkeyState(si())), fl.rest)

  val MonkeyHeader: Regex = "Monkey (\\d+):".r

  def monkeyHeader(s: String): Try[Int] = Try {
    s match
      case MonkeyHeader(n) => n.toInt
      case _ => throw new IllegalArgumentException("Not a header")
  }

  def parseFrom(s: String, initial: String) = Try {
    if (s.startsWith(initial)) s.substring(initial.length)
    else throw new IllegalArgumentException(s"Didn't start with '$initial'")
  }

  def parseOperation(s: String): Try[MonkeyFunction] = Try {
    // There are only 3 types of operation: add static, multiply static, or square
    if (s == "* old") {
      squaring(_: Long)
    } else if (s.startsWith("+ ")) {
      staticAddition((s.substring(2).toLong))
    } else if (s.startsWith("* ")) {
      staticMultiplication((s.substring(2).toLong))
    } else {
      throw new IllegalArgumentException("Bad string: " + s)
    }

  }

  class Monkey(val num: Int,
               operation: MonkeyFunction,
               val test: Long,
               trueDest: Int,
               falseDest: Int,
               divideByThree: Boolean,
               state: MonkeyState) {

    def processQueue(allMonkeys: Array[Monkey], modulo: Long): Unit = {
      @tailrec
      def process(): Unit = {
        state.getItem() match
          case Some(item) =>
            val newItem = (if (divideByThree) operation(item) / 3 else operation(item)) % modulo
            if (newItem % test == 0) allMonkeys(trueDest).receive(newItem)
            else allMonkeys(falseDest).receive(newItem)
            process()
          case None =>
            ()
      }

      process()
    }

    def receive(item: Long) = state.addItem(item)

    def handled: Long = state.getHandled
  }

  // Mutable
  class MonkeyState(initial: Seq[Long]) {
    private val queue: FifoQueue[Long] = new FifoQueue[Long]()
    private val handled: AtomicLong = new AtomicLong(0)

    queue.pushAll(initial)

    def getItem(): Option[Long] = if (queue.isEmpty) None else {
      handled.incrementAndGet()
      Some(queue.pop())
    }

    def addItem(item: Long) = queue.push(item)

    def getHandled: Long = handled.get()

  }

  type MonkeyFunction = Long => Long

  def staticAddition(x: Long)(old: Long): Long = old + x

  def staticMultiplication(x: Long)(old: Long): Long = old * x

  def squaring(old: Long): Long = old * old
}
