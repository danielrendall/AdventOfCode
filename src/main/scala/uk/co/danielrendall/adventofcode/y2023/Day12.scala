package uk.co.danielrendall.adventofcode.y2023

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*
import uk.co.danielrendall.adventofcode.y2023.Day12.CharString.{DotString, HashString, QuestionString}
import uk.co.danielrendall.adventofcode.y2023.Day12.TemplateRegion.{Certain, Forbidden, Uncertain}

import scala.annotation.tailrec

object Day12 {

  val testData: LazyList[String] =
    """???.### 1,1,3
      |.??..??...?##. 1,1,3
      |?#?#?#?#?#?#?#? 1,3,1,6
      |????.#...#... 4,1,1
      |????.######..#####. 1,6,5
      |?###???????? 3,2,1""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2023/day12.txt").lines

  @main def d12p1(): Unit = {

    def solve(list: LazyList[String]) =
      list.map(parse)
        .map { case (string, groups) => solvePart1(string, groups) }
        .sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d12p2(): Unit = {

    def solve(list: LazyList[String]) =
      list.map(parse)
        .map {
          case (miniString, miniGroups) =>
            val string = miniString * 5
            val groups = (0 until 5).flatMap(_ => miniGroups).toList

            solvePart2(string, groups)
        }.sum

    println("Test: " + solve(testData.take(1)))
    //    println("Actual: " + solve(numbers))
  }

  private def parse(line: String): (String, List[Int]) = {
    val bits = line.split(' ')
    (bits(0), bits(1).split(',').map(_.toInt).toList)
  }

  def solvePart1(template: String, groups: List[Int]): Long = {
    val unknownIndexes = template.zipWithIndex.filter(_._1 == '?').map(_._2)
    val indexMap = unknownIndexes.zipWithIndex.map { case (stringIndex, indexOfIndex) => indexOfIndex -> stringIndex }.toMap
    val unknownHashCount = groups.sum - template.count(_ == '#')

    if (unknownHashCount == 0) {
      1
    } else {
      populateUnknownIndexesWithHashes(indexMap.size, unknownHashCount).map { indexes =>
        val templateAsCharArray: Array[Char] = template.toCharArray
        indexes.map(indexMap.apply).foreach(i => templateAsCharArray(i) = '#')
        new String(templateAsCharArray)
      }.count(s => check(s, groups)).toLong
    }
  }

  def populateUnknownIndexesWithHashes(size: Int, hashesToPlace: Int): LazyList[List[Int]] = {
    if (hashesToPlace == 1) {
      ((size - 1) to(0, -1)).to(LazyList).map(i => List(i))
    } else {
      ((size - 1) to(hashesToPlace - 1, -1)).to(LazyList).flatMap { pos =>
        populateUnknownIndexesWithHashes(pos, hashesToPlace - 1).map(list => pos :: list)
      }
    }
  }

  def check(initialString: String, allGroups: List[Int]): Boolean = {

    @tailrec
    def recurse(string: String, groups: List[Int]): Boolean = {
      string.indexOf("#") match {
        case 0 => false // Should never start with a #; we need clear space between groups
        case -1 => groups.isEmpty // No more hashes - we need to have run out of groups
        case first =>
          groups match {
            case nextGroup :: rest =>
              val maybeLast = string.indexWhere(_ != '#', first)
              val last = if (maybeLast != -1) maybeLast else string.length()
              if ((last - first) == nextGroup) {
                recurse(string.substring(last), rest)
              } else {
                false
              }
            case _ =>
              // Found a group, but we aren't expecting one
              false
          }
      }
    }

    recurse("." + initialString, allGroups)

  }

  def solvePart2(template: String, groups: List[Int]): Long = {
    val indexed: List[IndexedGroup] = groupsToIndexed(groups)
    val regions: List[TemplateRegion] = (toCharStrings andThen toRegions)(template)
    solveForGroupsAndRegions(indexed, regions)
  }

  private def groupsToIndexed(groups: List[Int]): List[IndexedGroup] = {
    val totalHashes = groups.sum

    @tailrec
    def process(remaining: List[(Int, Int)], hashesEncountered: Int, accum: List[IndexedGroup]): List[IndexedGroup] =
      remaining match {
        case head :: rest =>
          val newHashesEncountered = hashesEncountered + head._1
          process(rest, newHashesEncountered, IndexedGroup(head._2, head._1, hashesEncountered, totalHashes - newHashesEncountered) :: accum)
        case _ =>
          accum.reverse
      }

    process(groups.zipWithIndex, 0, List.empty)
  }

  private def toCharStrings(template: String): List[CharString] = {

    val totalHashes = template.count(_ == '#')
    val totalNonDot = template.count(_ != '.')

    @tailrec
    def process(remaining: List[(Char, Int)],
                hashesSoFar: Int,
                nonDotSoFar: Int,
                accum: List[CharString]): List[CharString] =

      remaining match {
        case head :: rest =>
          val continuation = rest.takeWhile(_._1 == head._1)
          val contiguousRegion = head :: continuation

          val newHashesSoFar = hashesSoFar + (if (head._1 == '#') contiguousRegion.length else 0)
          val newNonDotSoFar = nonDotSoFar + (if (head._1 != '.') contiguousRegion.length else 0)
          val span = Span(head._2, head._2 + contiguousRegion.length, hashesSoFar, nonDotSoFar, totalHashes - newHashesSoFar, totalNonDot - newNonDotSoFar)
          val charString = head._1 match {
            case '.' => DotString(span)
            case '#' => HashString(span)
            case '?' => QuestionString(span)
          }
          process(rest.drop(continuation.length), newHashesSoFar, newNonDotSoFar, charString :: accum)
        case _ =>
          accum.reverse
      }

    process(template.toList.zipWithIndex, 0, 0, List.empty)
  }

  private def toRegions(charStrings: List[CharString]): List[TemplateRegion] = {

    def process(remaining: List[CharString],
                lastOpt: Option[CharString],
                accum: List[TemplateRegion]): List[TemplateRegion] = {
      remaining match
        case head :: rest =>
          head match {
            case d: DotString =>
              process(rest, Some(d), Forbidden(d) :: accum)
            case h: HashString =>
              lastOpt match {
                case Some(last: QuestionString) =>
                  accum.head match {
                    case u: Uncertain =>
                      process(rest, Some(h), u.add(h) :: accum.tail)
                    case _ =>
                      throw new IllegalStateException(s"Last was $last, accum head can't be ${accum.head}")
                  }
                case _ =>
                  process(rest, Some(h), Certain(h) :: accum)
              }

            case q: QuestionString =>
              lastOpt match {
                case Some(last: HashString) =>
                  accum.head match {
                    case u: Uncertain =>
                      process(rest, Some(q), u.add(q) :: accum.tail)
                    case c: Certain =>
                      process(rest, Some(q), Uncertain(c.span + q.span, List(c.hashString, q)) :: accum.tail)
                    case _ =>
                      throw new IllegalStateException(s"Last was $last, accum head can't be ${accum.head}")
                  }
                case _ =>
                  process(rest, Some(q), Uncertain(q.span, List(q)) :: accum)
              }
          }
        case _ =>
          accum.reverse
    }


    process(charStrings, None, List.empty)
  }

  private def solveForGroupsAndRegions(allGroups: List[IndexedGroup],
                                       allRegions: List[TemplateRegion]): Long = {

    @tailrec
    def process(groups: List[IndexedGroup],
                regions: List[TemplateRegion],
                accum: Long): Long = {
      regions match
        case region :: restOfRegions =>
          // What groups can this region accept...?
          region match
            case f: Forbidden =>
              // No groups can be matched here.
              process(groups, restOfRegions, accum)
            case c: Certain =>
              // We absolutely need to match the next group
              groups match
                case group :: restOfGroups =>
                  if (c.span.length == group.groupLength) {
                    process(restOfGroups, restOfRegions, accum)
                  } else {
                    0
                  }
                case _ =>
                  0
            case u: Uncertain =>
              // TODO
              u.getMatchingOptions(groups)
              0
        case _ =>
          // We've processed all regions; have we processed all groups...?
          if (groups.isEmpty) {
            1
          } else {
            // Didn't process everything
            0
          }
    }

    process(allGroups, allRegions, 1)
  }


  /**
   * @param index        Index of group in list of groups
   * @param groupLength  Length of hashes in the group (>= 1)
   * @param hashesBefore The number of hashes that must appear somehow before this group
   * @param hashesAfter  The number of hashes that must appear somehow after this group
   */
  case class IndexedGroup(index: Int,
                          groupLength: Int,
                          hashesBefore: Int,
                          hashesAfter: Int)

  /**
   * A span having particular characteristics
   *
   * @param startIndex          Start of the span (inclusive)
   * @param endIndex            End of the span (exclusive)
   * @param minimumHashesBefore The minimum number of hashes that can possibly appear before this
   * @param maximumHashesBefore The maximum number of hashes that could appear before this
   * @param minimumHashesAfter  The minimum number of hashes that can possibly appear after this
   * @param maximumHashesAfter  The maximum number of hashes that could appear after this
   */
  case class Span(startIndex: Int,
                  endIndex: Int,
                  minimumHashesBefore: Int,
                  maximumHashesBefore: Int,
                  minimumHashesAfter: Int,
                  maximumHashesAfter: Int) {
    def length: Int = endIndex - startIndex

    def +(other: Span) = {
      assert(other.startIndex == endIndex, "Cannot add non-contiguous spans")
      Span(startIndex, other.endIndex, minimumHashesBefore, maximumHashesBefore, other.minimumHashesAfter, other.maximumHashesAfter)
    }
  }

  sealed trait TemplateRegion {
    def span: Span
  }

  object TemplateRegion {

    /**
     * A region of the template which can't contain any hashes
     *
     * @param span The length covered
     */
    case class Forbidden(dotString: DotString) extends TemplateRegion {

      override def span: Span = dotString.span
    }

    /**
     * A region of the template which must be completely covered by hashes of the given length; note that a Certain
     * region cannot be adjacent to an Uncertain region
     *
     * @param span The length covered
     */
    case class Certain(hashString: HashString) extends TemplateRegion {
      override def span: Span = hashString.span
    }

    /**
     * A region of the template that consists of a mixture of hashes and question marks such that the possibilities for
     * what things the region contains are uncertain
     *
     * @param span    The length covered
     * @param strings The make-up of the region (strings of hashes and / or strings of question marks)
     */
    case class Uncertain(span: Span, strings: List[CharString]) extends TemplateRegion {

      def add(s: HashString | QuestionString) =
        Uncertain(span + s.span, strings :+ s)

      /**
       * We have the supplied groups, and we need to get all of the options for consuming some of the groups and
       * returning the rest.
       *
       * For example, if we can consume either one group in 4 ways, or two groups in 2 ways, we would return the list:
       * (4, groups.tail), (2, groups.tail.tail)
       *
       * @param groups
       * @return
       */
      def getMatchingOptions(groups: List[IndexedGroup]): List[(Int, List[IndexedGroup])] = {
        strings.headOption match {
          case Some(head) =>
            head match
              case h:HashString =>
                getMatchingOptions(h, strings.tail, true, groups)
              case q:QuestionString =>
                getMatchingOptions(q, strings.tail, true, groups)
              case _ =>
                throw new IllegalStateException("Head should have been a hash or a question string")
          case _ =>
            throw new IllegalStateException("Should have had some strings")
        }
      }

      def getMatchingOptions(h: HashString,
                             remaining: List[CharString],
                             isFirst: Boolean,
                             groups: List[IndexedGroup]): List[(Int, List[IndexedGroup])] = {
        ???
      }

      def getMatchingOptions(q: QuestionString,
                             remaining: List[CharString],
                             isFirst: Boolean,
                             groups: List[IndexedGroup]): List[(Int, List[IndexedGroup])] = {
        ???
      }
    }
  }

  sealed trait CharString {
    def span: Span
  }

  object CharString {
    case class DotString(span: Span) extends CharString

    case class HashString(span: Span) extends CharString

    case class QuestionString(span: Span) extends CharString
  }

  case class PossibleUncertainMatch(remainingGroups: List[IndexedGroup])
}

