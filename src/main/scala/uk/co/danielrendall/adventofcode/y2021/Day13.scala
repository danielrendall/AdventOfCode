package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day13 {

  val testData: LazyList[String] =
    """6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5""".stripMargin.splitAndTrimToList

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day13.txt").lines

  @main def day13p1()=
    def solve(seq: LazyList[String]): Unit =
      val (points, folds) = parseData(seq)
      val newPoints = performFold(folds.head, points)

      println(newPoints.size)


    solve(testData)
    solve(data)

  @main def day13p2()=
    def solve(seq: LazyList[String]): Unit =
      val (points, folds) = parseData(seq)
      val finalPoints: Set[Point] = folds.foldLeft(points) { case (points, fold) => performFold(fold, points)}
      val xs = finalPoints.map(_.x)
      val ys = finalPoints.map(_.y)
      val xMax = xs.max
      val yMax = ys.max
      val xRange = xs.min to xMax
      val yRange = ys.min to yMax


      val arr = new Array[Array[Char]](yMax + 1)
      for {
        y <- yRange
      } arr(y) = Array.fill(xMax + 1)(' ')

      finalPoints.foreach { p => arr(p.y)(p.x) = '#'}

      yRange.foreach(y => println(arr(y).mkString("")))



    solve(testData)
    println
    solve(data)


  def parseData(value: LazyList[String]): (Set[Point], Seq[Fold]) =
    val pointData = value.takeWhile(_.nonEmpty).map {s =>
      val bits = s.split(',')
      Point(bits(0).toInt, bits(1).toInt)
    }.toSet
    val foldData = value.dropWhile(_.nonEmpty).drop(1).takeWhile(_.nonEmpty).map(_.substring(11)).map { s =>
      val bits = s.split('=')
      bits(0) match {
        case "y" => Horizontal(bits(1).toInt)
        case "x" => Vertical(bits(1).toInt)
        case _ => throw new IllegalArgumentException("Bad bit: " + s)
      }
    }.toList
    (pointData, foldData)

  def performFold(fold: Fold, points: Set[Point]) = fold match {
    case Horizontal(at) =>
      points.map { p =>
        if (p.y < at) p
        else p.copy(y = 2 * at - p.y)
      }

    case Vertical(at) =>
      points.map { p =>
        if (p.x < at) p
        else p.copy(x = 2 * at - p.x)
      }
  }


  case class Point(x: Int, y: Int)

  sealed trait Fold {
    def at: Int
  }

  case class Horizontal(at: Int) extends Fold
  case class Vertical(at: Int) extends Fold

}
