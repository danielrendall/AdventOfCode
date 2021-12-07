package uk.co.danielrendall.adventofcode.utils

object OrderingUtils {

  extension[T] (pair: (T, T))
               (using ord: Ordering[T])
    def minMax: (T, T) = if (ord.lt(pair._1, pair._2)) pair else pair.swap

  extension[T] (iterable: Iterable[T])
               (using ord: Ordering[T])
    def minMax: Option[(T, T)] = iterable.headOption.map { h =>

      iterable.foldLeft((h, h)) { case ((curMin, curMax), next) =>
        (if (ord.lt(curMin, next)) curMin else next, if (ord.gt(curMax, next)) curMax else next)
      }

    }

}
