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

  def buildEmptyArray[T](width: Int,
                         height: Int,
                         initialValue: Loc => T,
                         borderValue: T)
                        (implicit ct: ClassTag[T]): Array2D[T] =
    // Add a border
    val firstAndLast = Array.fill[T](width + 2)(borderValue)
    val middle: Array[Array[T]] =
      (0 until height)
        .map(y => (0 until width)
          .map(x => initialValue(Loc(x, y))).toArray).map(a => (borderValue +: a :+ borderValue)).toArray
    val array = firstAndLast +: middle :+ firstAndLast
    Array2D(array, width, height)


  // Width and Height don't include the border
  case class Array2D[T](array: Array[Array[T]], width: Int, height: Int)
                       (implicit classTag: ClassTag[T]) {

    private lazy val borderLine: Array[T] = array.head
    lazy val borderValue: T = borderLine.head

    def locs: Seq[Loc] = for {
      y <- 1 to height
      x <- 1 to width
    } yield Loc(x, y)

    def locsAndValues: Seq[(Loc, T)] = for {
      y <- 1 to height
      x <- 1 to width
    } yield (Loc(x, y), array(y)(x))

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

    def mapValues[U](fn: T => U)
                    (implicit ctU: ClassTag[U]): Array2D[U] = {
      val newBorder = fn(borderValue)
      val newBorderLine = borderLine.map(fn)
      val middle = (for {
        y <- 1 to height
      } yield {
        (newBorder +: (for {
          x <- 1 to width
        } yield (fn(get(Loc(x, y))))) :+ newBorder).toArray[U]
      }).toArray[Array[U]]
      val newArray: Array[Array[U]] = newBorderLine +: middle :+ newBorderLine
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

    def max(implicit ordering: Ordering[T]): T = locs.map(get).max

    def min(implicit ordering: Ordering[T]): T = locs.map(get).min

    override def toString: String = toString(",")

    def toString(sep: String): String =
      (1 to height).map(array.apply).map { row =>
        (1 to width).map(row.apply).mkString(sep)
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

    // Beware - mutates the arrays - need to make sure they aren't the same object!
    def updateMut(loc: Loc, fn: T => T): Array2D[T] = {
      val cur = array(loc.y)(loc.x)
      array(loc.y)(loc.x) = fn(cur)
      this
    }

    // Mutates the array!
    def set(loc: Loc, t: T): Unit = {
      array(loc.y)(loc.x) = t
    }

    def expand: Array2D[T] = {
      val newWidth = width + 2
      val newHeight = height + 2
      Array2D.fill(newWidth, newHeight)(borderValue).map { loc =>
        get(loc.upLeft)
      }
    }

    def rowIteratorLR: LazyList[LazyList[Loc]] = {
      for (y <- numList(1, height)) yield {
        for (x <- numList(1, width)) yield Loc(x, y)
      }
    }

    def rowIteratorRL: LazyList[LazyList[Loc]] = {
      for (y <- numList(1, height)) yield {
        for (x <- numList(1, width)) yield Loc((width - x + 1), y)
      }
    }

    def columnIteratorTD: LazyList[LazyList[Loc]] = {
      for (x <- numList(1, width)) yield {
        for (y <- numList(1, height)) yield Loc(x, y)
      }
    }

    def columnIteratorDT: LazyList[LazyList[Loc]] = {
      for (x <- numList(1, width)) yield {
        for (y <- numList(1, height)) yield Loc(x, (height - y + 1))
      }
    }


    def zip[U](other: Array2D[U]): Array2D[(T, U)] = {
      assert(width == other.width && height == other.height)
      val newArray = Array2D.fill(width, height)((borderValue, other.borderValue))
      locs.foreach(loc => newArray.set(loc, (get(loc), other.get(loc))))
      newArray
    }

    def insertRow(idx: Int, value: T): Array2D[T] = {
      val newHeight = height + 1
      Array2D.fill(width, newHeight)(borderValue).map { loc =>
        if (loc.y <= idx)
          get(loc)
        else
          get(loc.up)
      }
    }

    def insertCol(idx: Int, value: T): Array2D[T] = {
      val newWidth = width + 1
      Array2D.fill(newWidth, height)(borderValue).map { loc =>
        if (loc.x <= idx)
          get(loc)
        else
          get(loc.left)
      }
    }


    private def numList(start: Int, inclusiveEnd: Int): LazyList[Int] =
      (start to inclusiveEnd).to(LazyList)

    def get(loc: Loc): T = array(loc.y)(loc.x)

    def count(pred: T => Boolean): Int = locs.count(l => pred(get(l)))

    def withNewBorder(newBorder: T): Array2D[T] = map(loc => get(loc), newBorder)

    // Returned true if loc is in the non-bordered part of this array
    def contains(loc: Loc): Boolean = loc.x >= 1 && loc.x <= width && loc.y >= 1 && loc.y <= height

    def findLocs(pred: T => Boolean): Seq[Loc] = locs.filter(l => pred(get(l)))
  }

  object Array2D {

    def fill[T](width: Int, height: Int)
               (init: T)
               (implicit ct: ClassTag[T]): Array2D[T] = {
      val emptyRow = List.fill[T](width + 2)(init)
      Array2D(LazyList.continually(emptyRow.toArray).take(height + 1).toArray, width, height)
    }


    // TODO - implement me
    implicit def Array2DIsNumeric[T: Numeric](implicit classTag: ClassTag[T]): Numeric[Array2D[T]] = new Numeric[Array2D[T]] {
      val numericForT: Numeric[T] = implicitly[Numeric[T]]

      override def plus(x: Array2D[T], y: Array2D[T]): Array2D[T] = ???

      override def minus(x: Array2D[T], y: Array2D[T]): Array2D[T] = ???

      override def times(x: Array2D[T], y: Array2D[T]): Array2D[T] = x.zip(y).mapValues((x, y) => numericForT.times(x, y))

      override def negate(x: Array2D[T]): Array2D[T] = ???

      override def fromInt(x: Int): Array2D[T] = ???

      override def parseString(str: String): Option[Array2D[T]] = ???

      override def toInt(x: Array2D[T]): Int = ???

      override def toLong(x: Array2D[T]): Long = ???

      override def toFloat(x: Array2D[T]): Float = ???

      override def toDouble(x: Array2D[T]): Double = ???

      override def compare(x: Array2D[T], y: Array2D[T]): Int = ???
    }

  }

  case class Loc(x: Int, y: Int) {

    def apply(move: Move): Loc = move match {
      case Move.NoMove => this
      case Move.Up(distance) => up(distance)
      case Move.UpLeft(distance) => upLeft(distance)
      case Move.Left(distance) => left(distance)
      case Move.DownLeft(distance) => downLeft(distance)
      case Move.Down(distance) => down(distance)
      case Move.DownRight(distance) => downRight(distance)
      case Move.Right(distance) => right(distance)
      case Move.UpRight(distance) => upRight(distance)
    }

    def up: Loc = up(1)

    def up(dist: Int): Loc = copy(y = y - dist)

    def down: Loc = down(1)

    def down(dist: Int): Loc = copy(y = y + dist)

    def left: Loc = left(1)

    def left(dist: Int): Loc = copy(x = x - dist)

    def right: Loc = right(1)

    def right(dist: Int): Loc = copy(x = x + dist)

    def upLeft: Loc = upLeft(1)

    def upLeft(dist: Int): Loc = Loc(x - dist, y - dist)

    def upRight: Loc = upRight(1)

    def upRight(dist: Int): Loc = Loc(x + dist, y - dist)

    def downLeft: Loc = downLeft(1)

    def downLeft(dist: Int): Loc = Loc(x - dist, y + dist)

    def downRight: Loc = downRight(1)

    def downRight(dist: Int): Loc = Loc(x + dist, y + dist)

    def gridAdjacent: Seq[Loc] = Seq(up, right, down, left)

    def allAdjacent: Seq[Loc] = Seq(up, upRight, right, downRight, down, downLeft, left, upLeft)

    def get[T](array2D: Array2D[T]): T = array2D.array(y)(x)
  }

  sealed abstract class LinearDirection(val op: Loc => Loc, _right: => LinearDirection) {
    final lazy val right: LinearDirection = _right
    final lazy val back: LinearDirection = right.right
    final lazy val left: LinearDirection = back.right

    final lazy val opposite: LinearDirection = back

    def apply(loc: Loc): Loc = op(loc)

    final lazy val move: Move = this match
      case LinearDirection.Up => Move.up
      case LinearDirection.Right => Move.right
      case LinearDirection.Down => Move.down
      case LinearDirection.Left => Move.left
  }

  object LinearDirection {
    case object Up extends LinearDirection(_.up, Right)

    case object Right extends LinearDirection(_.right, Down)

    case object Down extends LinearDirection(_.down, Left)

    case object Left extends LinearDirection(_.left, Up)

    val all = Seq(Up, Right, Down, Left)
  }

  sealed abstract class CompassDirection(val op: Loc => Loc, _forwardRight: => CompassDirection) {
    final lazy val forwardRight = _forwardRight
    final lazy val right = forwardRight.forwardRight
    final lazy val backRight = right.forwardRight
    final lazy val back = backRight.forwardRight
    final lazy val backLeft = back.forwardRight
    final lazy val left = backLeft.forwardRight
    final lazy val forwardLeft = left.forwardRight

    final lazy val opposite: CompassDirection = back

    final lazy val move: Move = this match
      case CompassDirection.Up => Move.up
      case CompassDirection.UpRight => Move.upRight
      case CompassDirection.Right => Move.right
      case CompassDirection.DownRight => Move.downRight
      case CompassDirection.Down => Move.down
      case CompassDirection.DownLeft => Move.downLeft
      case CompassDirection.Left => Move.left
      case CompassDirection.UpLeft => Move.upLeft
  }

  object CompassDirection {
    case object Up extends CompassDirection(_.up, UpRight)

    case object UpRight extends CompassDirection(_.upRight, Right)

    case object Right extends CompassDirection(_.right, DownRight)

    case object DownRight extends CompassDirection(_.downRight, Down)

    case object Down extends CompassDirection(_.down, DownLeft)

    case object DownLeft extends CompassDirection(_.downLeft, Left)

    case object Left extends CompassDirection(_.left, UpLeft)

    case object UpLeft extends CompassDirection(_.upLeft, Up)

    val all: Seq[CompassDirection] = Seq(Up, UpRight, Right, DownRight, Down, DownLeft, Left, UpLeft)
    val rectilinear: Seq[CompassDirection] = Seq(Up, Right, Down, Left)
    val diagonal: Seq[CompassDirection] = Seq(UpRight, DownRight, DownLeft, UpLeft)
  }
}
