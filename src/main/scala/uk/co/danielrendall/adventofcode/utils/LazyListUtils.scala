package uk.co.danielrendall.adventofcode.utils

/**
 * <Insert Description Here>
 *
 */
object LazyListUtils {

  extension[A] (list: Seq[A])
    def windowed(size: Int): LazyList[Seq[A]] =
      def getGroup =
        val seq = list.take(size)
        if (seq.size == size) Some(seq) else None

      getGroup.map(seq =>  seq #:: list.tail.windowed(size)).getOrElse(LazyList.empty)

}
