package uk.co.danielrendall.adventofcode.utils

import scala.reflect.ClassTag

object ArrayUtils {

  def buildBorderedArray[T](seq: LazyList[String],
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


  case class Array2D[T](array: Array[Array[T]], width: Int, height: Int) {

    def locs: Seq[Loc] = for {
      y <- 1 to height
      x <- 1 to width
    } yield Loc(x, y)

    override def toString: String = array.map(_.mkString(",")).mkString("\n")

  }

  case class Loc(x: Int, y: Int) {
    def up: Loc = copy(y = y - 1)

    def down: Loc = copy(y = y + 1)

    def left: Loc = copy(x = x - 1)

    def right: Loc = copy(x = x + 1)

    def get[T](array2D: Array2D[T]): T = array2D.array(y)(x)
  }

}
