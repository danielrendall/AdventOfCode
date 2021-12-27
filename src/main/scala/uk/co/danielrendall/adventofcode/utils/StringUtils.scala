package uk.co.danielrendall.adventofcode.utils

object StringUtils {

  extension (string: String)
    def splitAndTrimToList: LazyList[String] = splitToList.map(_.trim)
    def splitToList: LazyList[String] = string.split('\n').to(LazyList)

}
