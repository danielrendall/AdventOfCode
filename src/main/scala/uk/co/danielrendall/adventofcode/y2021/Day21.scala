package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day21 {

  val testData: LazyList[String] =
    """Player 1 starting position: 4
      |Player 2 starting position: 8""".stripMargin.splitAndTrimToList

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day21.txt").lines

  @main def d21p1() =
    def solve(seq: Seq[String]): Unit =
      val positions: Seq[DeterministicPlayerState] = getInitialPositions(seq)
      val p1 = positions.head
      val p2 = positions.tail.head

      // Returns rollCount, winner, loser
      @tailrec
      def process(thisPlayer: DeterministicPlayerState,
                  otherPlayer: DeterministicPlayerState,
                  dice: LazyList[(Int, Int)]): (Int, DeterministicPlayerState, DeterministicPlayerState) = {
        dice match {
          case r1 #:: r2 #:: r3 #:: rest =>
            val spaces: Int = r1._1 + r2._1 + r3._1
            val nextStateForThisPlayer = thisPlayer.move(spaces)
            if (nextStateForThisPlayer.hasWon) {
              (rest.head._2, nextStateForThisPlayer, otherPlayer)
            } else {
              process(otherPlayer, nextStateForThisPlayer, rest)
            }
        }
      }

      val dice: LazyList[(Int, Int)] = LazyList.continually((1 to 100).to(LazyList)).flatten.zipWithIndex

      val (rollCount, winner, loser) = process(p1, p2, dice)

      println(rollCount)
      println("Total: " + (loser.positionAndScore.score * rollCount))

    solve(testData)
    solve(data)


  @main def d21p2() =
    def solve(seq: Seq[String]): Unit =
      val positions: Seq[DeterministicPlayerState] = getInitialPositions(seq)
      val p1 = positions.head
      val p2 = positions.tail.head

      val initialState = QuantumGameState.initial(p1, p2)

      val game: LazyList[QuantumGameState] = LazyList.unfold(initialState) { state =>
        if (state.shouldFinish) {
          None
        } else {
          val updated = state.updateState
          Some((updated, updated))
        }
      }

      val finalState = game.last
      println(finalState.p1Wins)
      println(finalState.p2Wins)


    solve(testData)
    solve(data)

  // Map of dice outcome -> number of universes in which it happens
  val quantumDiceOutcomes: Seq[(Int, Int)] = (for {
    a <- (1 to 3)
    b <- (1 to 3)
    c <- (1 to 3)
  } yield (a + b + c)).groupBy(identity).view.mapValues(_.size).toSeq.sortBy(_._1)

  val LineRegex: Regex = "^Player ([0-9]+) starting position: ([0-9]+)$".r

  def getInitialPositions(seq: Seq[String]): Seq[DeterministicPlayerState] =
    seq.zipWithIndex.collect {
      case (LineRegex(pNum, pos), idx) => DeterministicPlayerState(idx + 1, PositionAndScore.initial(pos.toInt))
    }

  case class DeterministicPlayerState(player: Int, positionAndScore: PositionAndScore) {

    def hasWon: Boolean = positionAndScore.isWin

    def move(spaces: Int): DeterministicPlayerState = copy(positionAndScore = positionAndScore.move(spaces))

  }

  case class QuantumGameState(nextToPlay: Int,
                              possibilities: Seq[(PossiblePositionAndScore, BigInt)],
                              p1Wins: BigInt,
                              p2Wins: BigInt) {

    def shouldFinish: Boolean = possibilities.isEmpty

    val numberOfUniverses: BigInt = possibilities.map(_._2).sum

    println("Currently " + numberOfUniverses + " universes, " + nextToPlay + " to play")

    def updateState: QuantumGameState = {
      if (nextToPlay == 1) {
        updateStateP1
      } else {
        updateStateP2
      }
    }

    def updateStateP1: QuantumGameState = {
      val afterUpdatingP1 = possibilities.flatMap { case (pp, numberOfUniverses) =>
        quantumDiceOutcomes.map { case (diceRoll, numberOfUniversesInWhichThatDiceRollOccurs) =>
          val updatedP1Position = pp.p1.move(diceRoll)
          pp.copy(p1 = updatedP1Position) -> (numberOfUniverses * numberOfUniversesInWhichThatDiceRollOccurs)
        }
      }
      val (wins, continues) = afterUpdatingP1.partition(_._1.p1.score >= 21)
      val newWins = wins.map(_._2).sum

      val grouped = continues.groupBy(_._1).view.mapValues(_.map(_._2).sum).toSeq

      QuantumGameState(2, grouped, p1Wins + newWins, p2Wins)
    }

    def updateStateP2: QuantumGameState = {
      val afterUpdatingP2 = possibilities.flatMap { case (pp, numberOfUniverses) =>
        quantumDiceOutcomes.map { case (diceRoll, numberOfUniversesInWhichThatDiceRollOccurs) =>
          val updatedP2Position = pp.p2.move(diceRoll)
          pp.copy(p2 = updatedP2Position) -> (numberOfUniverses * numberOfUniversesInWhichThatDiceRollOccurs)
        }
      }
      val (wins, continues) = afterUpdatingP2.partition(_._1.p2.score >= 21)
      val newWins = wins.map(_._2).sum

      QuantumGameState(1, continues, p1Wins, p2Wins + newWins)
    }

  }

  object QuantumGameState {
    def initial(p1: DeterministicPlayerState, p2: DeterministicPlayerState): QuantumGameState = {
      val initialPossibility: Seq[(PossiblePositionAndScore, BigInt)] = Seq(((PossiblePositionAndScore(p1.positionAndScore, p2.positionAndScore)), BigInt(1)))
      QuantumGameState(1,
        initialPossibility,
        BigInt(0),
        BigInt(0))
    }
  }

  case class PossiblePositionAndScore(p1: PositionAndScore, p2: PositionAndScore)

  case class PositionAndScore(pos: Int, score: Int) {

    def move(spaces: Int): PositionAndScore = {
      @tailrec
      def reduce(n: Int): Int = {
        if (n <= 10) n else reduce(n - 10)
      }
      val newPos = reduce(pos + spaces)
      PositionAndScore(newPos, score + newPos)
    }

    override def toString: String = "Position: " + pos + " score: " + score

    def isWin: Boolean = score >= 1000

  }

  object PositionAndScore {
    def initial(pos: Int): PositionAndScore = PositionAndScore(pos, 0)
  }

}
