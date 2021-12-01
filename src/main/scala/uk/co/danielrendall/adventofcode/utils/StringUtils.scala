package uk.co.danielrendall.adventofcode.utils

object StringUtils {

  extension (string: String)
    def splitAndTrimToList: LazyList[String] = string.split('\n').map(_.trim).to(LazyList)

}
