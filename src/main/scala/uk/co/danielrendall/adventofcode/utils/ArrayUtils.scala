package uk.co.danielrendall.adventofcode.utils

import scala.reflect.ClassTag

object ArrayUtils {

  def buildBorderedArray[T](seq: Seq[String],
                            charToT: Char => T,
                            borderValue: T)
                           (implicit ct: ClassTag[T]): Array2D[T] =
    // Add a border
    val width = seq.head.length
    val firstAndLast = Array.fill[T](width + 2)(borderValue)
    val middle = seq.map(_.toCharArray.map(charToT)).map(a => (borderValue +: a :+ borderValue))
    val array = firstAndLast +: middle.toArray :+ firstAndLast
    val height = array.length - 2
    Array2D(array, width, height)


  case class Array2D[T](array: Array[Array[T]], width: Int, height: Int)
                       (implicit classTag: ClassTag[T]) {

    private lazy val borderLine: Array[T] = array.head
    private lazy val borderValue: T = borderLine.head

    def locs: Seq[Loc] = for {
      y <- 1 to height
      x <- 1 to width
    } yield Loc(x, y)

    def map(fn: Loc => T): Array2D[T] = {
      val middle = (for {
        y <- 1 to height
      } yield {
        (borderValue +: (for {
          x <- 1 to width
        } yield (fn(Loc(x, y)))) :+ borderValue).toArray[T]
      }).toArray[Array[T]]
      val newArray: Array[Array[T]] = borderLine +: middle :+ borderLine
      Array2D(newArray, width, height)
    }

    def map[W](fn: Loc => W,
               borderValue: W)
              (implicit classTag: ClassTag[W]): Array2D[W] = {
      val firstAndLast = Array.fill[W](width + 2)(borderValue)
      val middle = (for {
        y <- 1 to height
      } yield {
        (borderValue +: (for {
          x <- 1 to width
        } yield (fn(Loc(x, y)))) :+ borderValue).toArray[W]
      }).toArray[Array[W]]
      val newArray: Array[Array[W]] = firstAndLast +: middle :+ firstAndLast
      Array2D(newArray, width, height)
    }

    override def toString: String =
      (1 to height).map(array.apply).map { row =>
        (1 to width).map(row.apply).mkString(",")
      }.mkString("\n")

    def update(loc: Loc, fn: T => T): Array2D[T] = {
      val updated: Array[Array[T]] = array.zipWithIndex.map { case (row, rowIndex) =>
        if (rowIndex != loc.y) row
        else row.zipWithIndex.map { case (cell, columnIndex) =>
          if (columnIndex != loc.x) cell
            else fn(cell)
        }
      }
      copy(array = updated)
    }

    def updateMut(loc: Loc, fn: T => T): Array2D[T] = {
      val cur = array(loc.y)(loc.x)
      array(loc.y)(loc.x) = fn(cur)
      this
    }

    def get(loc: Loc): T = array(loc.y)(loc.x)

    // Returned true if loc is in the non-bordered part of this array
    def contains(loc: Loc): Boolean = loc.x >= 1 && loc.x <= width && loc.y >= 1 && loc.y <= height

  }

  case class Loc(x: Int, y: Int) {
    def up: Loc = copy(y = y - 1)

    def down: Loc = copy(y = y + 1)

    def left: Loc = copy(x = x - 1)

    def right: Loc = copy(x = x + 1)

    def upLeft: Loc = Loc(x - 1, y - 1)

    def upRight: Loc = Loc(x + 1, y - 1)

    def downLeft: Loc = Loc(x - 1, y + 1)

    def downRight: Loc = Loc(x + 1, y + 1)

    def gridAdjacent: Seq[Loc] = Seq(up, right, down, left)

    def allAdjacent: Seq[Loc] = Seq(up, upRight, right, downRight, down, downLeft, left, upLeft)

    def get[T](array2D: Array2D[T]): T = array2D.array(y)(x)
  }

}
