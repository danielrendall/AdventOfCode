package uk.co.danielrendall.adventofcode.utils

object OrderingUtils {

  extension[T] (pair: (T, T))
               (using ord: Ordering[T])
    def minMax: (T, T) = if (ord.lt(pair._1, pair._2)) pair else pair.swap

}
