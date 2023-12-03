package uk.co.danielrendall.adventofcode.y2023

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec
import scala.collection.immutable.LazyList

object Day2 {

  val testData: LazyList[String] =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2023/day2.txt").lines

  @main def d2p1(): Unit = {
    val (redMax, greenMax, blueMax) = (12, 13, 14)

    def solve(list: LazyList[String]) =
      list.flatMap(s => {
        val b1 = s.split(':').map(_.trim)
        val gameNum = b1.head.split(' ')(1).toInt

        @tailrec
        def findPossiblesInGame(rem: List[String]): Option[Int] = {
          rem match
            case head :: rest =>

              @tailrec
              def checkCountsArePossibleInGame(remInner: List[String]): Boolean = {
                remInner match
                  case headInner :: restInner =>
                    val bits = headInner.split(' ')
                    val count = bits.head.toInt
                    if ((bits(1) == "red" && count > redMax) ||
                      (bits(1) == "green" && count > greenMax) ||
                      (bits(1) == "blue" && count > blueMax)) {
                      false
                    } else {
                      checkCountsArePossibleInGame(restInner)
                    }
                  case _ =>
                    true
              }

              if (checkCountsArePossibleInGame(head.split(',').map(_.trim).toList)) {
                findPossiblesInGame(rest)
              } else {
                None
              }
            case _ =>
              Some(gameNum)
        }

        findPossiblesInGame(b1(1).split(';').map(_.trim).toList)
      }).sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }


  @main def d2p2(): Unit = {
    def solve(list: LazyList[String]) = {
      list.map(s => {

        @tailrec
        def getPower(remaining: List[String], redMax: Int, greenMax: Int, blueMax: Int): Int = {

          remaining match {
            case head :: rest =>

              @tailrec
              def getMaxInGame(remInner: List[String],
                               iRedMax: Int,
                               iGreenMax: Int,
                               iBlueMax: Int): (Int, Int, Int) = {
                remInner match {
                  case headInner :: restInner =>
                    val bits = headInner.split(' ')
                    val count = bits.head.toInt
                    bits(1) match {
                      case "red" =>
                        getMaxInGame(restInner, Math.max(iRedMax, count), iGreenMax, iBlueMax)
                      case "green" =>
                        getMaxInGame(restInner, iRedMax, Math.max(iGreenMax, count), iBlueMax)
                      case "blue" =>
                        getMaxInGame(restInner, iRedMax, iGreenMax, Math.max(iBlueMax, count))
                    }

                  case _ =>
                    (iRedMax, iGreenMax, iBlueMax)
                }
              }

              val (newRed, newGreen, newBlue) = getMaxInGame(head.split(',').map(_.trim).toList, redMax, greenMax, blueMax)
              getPower(rest, newRed, newGreen, newBlue)

            case _ =>
              redMax * greenMax * blueMax
          }

        }

        val b1 = s.split(':').map(_.trim)

        getPower(b1(1).split(';').map(_.trim).toList, 0, 0, 0)

      }).sum
    }

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }
}
