package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day4 {

  val testData: LazyList[String] =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
      |
      |22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19
      |
      | 3 15  0  2 22
      | 9 18 13 17  5
      |19  8  7 25 23
      |20 11 10 24  4
      |14 21 16 12  6
      |
      |14 21 17 24  4
      |10 16 15  9 19
      |18  8 23 26 20
      |22 11 13  6  5
      | 2  0 12  3  7""".stripMargin.splitAndTrimToList

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day4.txt").lines
  val boardRows = 5
  val boardCols = 5

  val rowNums: Range = (0 until boardRows)
  val colNums: Range = (0 until boardCols)

  @main def d4p1(): Unit =
    data.headOption match {
      case Some(numList) =>
        val numbers = numList.split(",").map(_.trim.toInt)
        val boards: Seq[Board] = data.tail.grouped(6).map { list =>
          Board(list.slice(1, 6).map(_.trim).flatMap(_.split("\\s+").map(n => Number(n.toInt, false))).toList)
        }.toSeq

        val finalState = numbers.foldLeft(State(boards)) { case (state, num) => state.update(num) }
        finalState.winnerOpt match {
          case Some(winner) =>
            val sumOfUnmatched = winner.unmatched.sum
            println("Sum of unmatched = " + sumOfUnmatched)
            println("Last called = " + finalState.lastCalled)
            println("Result = " + sumOfUnmatched * finalState.lastCalled)
          case None =>
            println("No winner")
        }

      case None =>
        // Fail
    }

  @main def d4p2(): Unit =
    data.headOption match {
      case Some(numList) =>
        val numbers = numList.split(",").map(_.trim.toInt)
        val boards: Seq[Board] = data.tail.grouped(6).map { list =>
          Board(list.slice(1, 6).map(_.trim).flatMap(_.split("\\s+").map(n => Number(n.toInt, false))).toList)
        }.toSeq

        val finalState = numbers.foldLeft(StateMisere(boards)) { case (state, num) => state.update(num) }
        finalState.lastWinnerOpt match {
          case Some(winner) =>
            val sumOfUnmatched = winner.unmatched.sum
            println("Sum of unmatched = " + sumOfUnmatched)
            println("Last called = " + finalState.lastCalled)
            println("Result = " + sumOfUnmatched * finalState.lastCalled)
          case None =>
            println("No winner")
        }

      case None =>
        // Fail
    }

  case class State(boards: Seq[Board], lastCalled: Int, winnerOpt: Option[Board]) {

    def update(num: Int): State = winnerOpt match {
      case Some(winner) => this
      case None => {
        val updatedBoards: Seq[Board] = boards.map(_.update(num))
        State(updatedBoards, num, updatedBoards.find(_.isWin))
      }
    }

  }

  object State {
    def apply(boards: Seq[Board]): State = State(boards, -1, None)
  }

  case class StateMisere(boardsRemaining: Seq[Board], boardsWon: List[Board], lastCalled: Int, lastWinnerOpt: Option[Board]) {

    def update(num: Int): StateMisere  = lastWinnerOpt match {
      case Some(winner) => this
      case None => {
        val updatedBoards: Seq[Board] = boardsRemaining.map(_.update(num))
        updatedBoards.headOption match {
          case Some(head) if head.isWin && updatedBoards.tail.isEmpty =>
            StateMisere(Seq.empty, head :: boardsWon, num, Some(head))
          case _ =>
            val (won, remaining) = updatedBoards.partition(_.isWin)
            StateMisere(remaining, won.toList ::: boardsWon, num, None)
        }
      }
    }

  }

  object StateMisere {
    def apply(boards: Seq[Board]): StateMisere = StateMisere(boards, List.empty, -1, None)
  }

  case class Board(nums: List[Number]) {

    def update(chosenNum: Int): Board = {
      def recurse(remaining: List[Number], accum: List[Number]): List[Number] = {
        remaining match {
          case h::l =>
            if (h.n == chosenNum) {
              if (!h.m) {
                  recurse(l, h.copy(m = true) :: accum)
              } else throw new IllegalStateException(s"Number $chosenNum already used")

            } else {
              recurse(l, h :: accum)
            }
          case _ => accum.reverse
        }
      }
      Board(recurse(nums, List.empty))

    }

    def unmatched: Seq[Int] = nums.filterNot(_.m).map(_.n)

    def isWin: Boolean = rowNums.exists(r => row(r).forall(_.m)) || colNums.exists(c => col(c).forall(_.m))

    def row(n: Int): Seq[Number] = nums.slice(n * boardCols, n * boardCols + boardRows)

    def col(n: Int): Seq[Number] = rowNums.map(_ * boardCols + n).map(nums.apply)

    override def toString: String = rowNums.map(n => row(n).map(n => "%02d".format(n.n)).mkString(" ")).mkString("\n")

  }

  case class Number(n: Int, m: Boolean)
}
