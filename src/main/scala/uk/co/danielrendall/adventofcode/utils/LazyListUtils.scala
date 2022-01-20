package uk.co.danielrendall.adventofcode.utils

object LazyListUtils {

  extension[A] (list: Seq[A])

  /**
   * Group the terms in the list into sequences of the given size, the sequences starting on consecutive members of the
   * list, and stopping as soon as it's not possible to return a complete sequence.
   *
   * If the list is (A, B, C, D, E) then windowing with size 3 will return ((A, B, C), (B, C, D), (C, D, E))
   */
    def windowed(size: Int): LazyList[Seq[A]] =
      def getGroup =
        val seq = list.take(size)
        if (seq.size == size) Some(seq) else None

      getGroup.map(seq =>  seq #:: list.tail.windowed(size)).getOrElse(LazyList.empty)

}
