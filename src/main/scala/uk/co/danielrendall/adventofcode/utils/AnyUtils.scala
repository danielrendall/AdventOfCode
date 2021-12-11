package uk.co.danielrendall.adventofcode.utils

object AnyUtils {

  extension[A] (a: A)
    def tuple2: (A, A) = (a, a)

}
