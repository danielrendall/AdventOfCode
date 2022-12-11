package uk.co.danielrendall.adventofcode.utils

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.{Failure, Try}

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

      getGroup.map(seq => seq #:: list.tail.windowed(size)).getOrElse(LazyList.empty)

    /**
     * Group the items by some separator
     *
     * @param separator
     * @return
     */
    def groupSeparatedBy(separator: A): LazyList[Seq[A]] =
      LazyList.unfold(list) { l =>
        val seq = l.takeWhile(_ != separator)
        if (seq.nonEmpty) {
          val rem = l.drop(seq.length)
          if (rem.nonEmpty) {
            Some((seq, rem.tail))
          } else {
            Some((seq, LazyList.empty))
          }
        } else {
          None
        }
      }

    /**
     * Group the items into clumps where the first item in each clump matches the predicate
     *
     * @param newGroup
     * @return
     */
    def groupStartingWith(newGroup: A => Boolean): LazyList[Seq[A]] =
      LazyList.unfold(list) { l =>

        @tailrec
        def readSeq(theList: Seq[A], accum: List[A]): (List[A], Seq[A]) =
          theList.headOption match
            case Some(value) =>
              if (newGroup(value)) {
                if (accum.isEmpty) {
                  readSeq(theList.tail, List(value))
                } else {
                  (accum.reverse, theList)
                }
              } else {
                readSeq(theList.tail, value :: accum)
              }
            case None =>
              (accum.reverse, Seq.empty)

        val (toReturn, rest) = readSeq(l, List.empty)
        if (toReturn.nonEmpty) {
          if (rest.nonEmpty) {
            Some((toReturn, rest))
          } else {
            Some((toReturn, LazyList.empty))

          }
        } else {
          None
        }
      }

    def toBorderedArray[T](fn: Char => T, borderValue: T)
                          (implicit ct: ClassTag[T],
                          ev: A =:= String): ArrayUtils.Array2D[T] =
      ArrayUtils.buildBorderedArray(list.asInstanceOf[Seq[String]], fn, borderValue)

    def parse[T](fn: A => Try[T]): Try[ParseResult[A, T]] =
      list.headOption match
        case Some(value) =>
          fn(value).map(r => ParseResult(r, list.tail))
        case None => Failure(new NoSuchElementException("Out of elements"))


  case class ParseResult[A, T](result: T, rest: Seq[A]):

    def apply() = result
    def parse[U](fn: A => Try[U]): Try[ParseResult[A, U]] = rest.parse(fn)
}
