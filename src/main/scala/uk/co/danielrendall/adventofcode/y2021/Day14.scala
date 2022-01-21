package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.CountingMap
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*
import uk.co.danielrendall.adventofcode.y2021.Day13.{data, parseData, performFold, testData}

object Day14 {

  /**
   * String processing. Start with a string of letters, repeatedly apply substitution rules to insert a new letter
   * between each pair of letters. Then find the difference between the most common and the least common letter
   *
   * Part 1
   * After 10 steps
   *
   * Part 2
   * After 40 steps
   */

  val testData: LazyList[String] =
    """NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C""".stripMargin.splitAndTrimToList

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day14.txt").lines

  @main def d14p1() =

    def solve(seq: LazyList[String]): Int =
      val (initial, substitutions) = parseData(seq)

      val substitutionMap = substitutions.map(_.toTupleMapEntry).toMap

      // This is a very naive solution; we keep track of the current string. At each step, we zip the string with its
      // tail to give a list of char pairs, we map each of these to the string "(first char of pair)(char that should be
      // inserted between them)" except for the last special case one, then we join all of these strings together to
      // make the next state
      def substitute(current: String): Option[(String, String)] = {
        // By adding a space, we ensure that the last char of the string will ZIP to make the pair ('char', ' ') and
        // this will trigger the "None" match below. Alternatively, we could not bother, and then just remember to
        // append the last character of the current string to the new string, since this can never change.
        // We're going to use this in LazyList.unfold, and we just return the current state as the computation result
        // (we could compute the letters and counts lazily, I suppose)
        val withSpace = current + " "
        val result = withSpace.zip(withSpace.tail).map { pair =>
          substitutionMap.get(pair) match {
            // So e.g. if pair is "AB" and we should insert a "C", we return the string "AC". We haven't lost the "B",
            // it will be the first char of the next pair to be processed.
            case Some(newChar) => s"${pair._1}$newChar"
            // We expect this case at the very end of the string
            case None => s"${pair._1}"
          }
        }.mkString.trim
        Some((result, result))
      }

      // Get the 10th state of the system
      val finalState = LazyList.unfold(initial)(substitute).drop(9).head

      // Get counts of all the characters, find minimum and maximum occurring
      val stats = finalState.toSet.map(test => (test, finalState.count(c => c == test)))
      val min = stats.minBy(_._2)._2
      val max = stats.maxBy(_._2)._2
      (max - min)

    println("Test: " +  solve(testData))
    println("Actual: " + solve(data))

  @main def d14p2() =
    def solve(seq: LazyList[String]): BigInt =
      val (initial, substitutions) = parseData(seq)
      val substitutionMap: Map[String, Seq[String]] = substitutions.map(_.toNewPairsMapEntry).toMap

      // It is apparent that the strings involved will get very long, therefore the naive approach of keeping track of
      // the string just won't work. Instead, we treat the state as a collection of counts of all of the 2-character
      // strings in it. As above, we return the state itself as the result of the computation, as well as being the
      // next state
      // We use a counting map to store the counts.
      def substitute(current: CountingMap[String, BigInt]): Option[(CountingMap[String, BigInt], CountingMap[String, BigInt])] = {

        // To get the new counts, we iterate through the current counts. Each string that we currently have will spawn
        // two new strings, as given by the substitution map; we add each of these to the new map with the count that
        // the parent string has in the current map
        val newCounts = current.map.foldLeft(CountingMap[String, BigInt]()) { case (map, (pair, count)) =>
          substitutionMap.get(pair) match {
            case Some(newPairs) =>
              newPairs.foldLeft(map) { case (m, p) => m.add(p, count)}
            case _ =>
              map.add(pair, count)
          }
        }
        Some((newCounts, newCounts))
      }

      // Prep; we zip the string with its tail and turn the pairs into the initial 2-character strings (noting that a
      // particular string may occur more than once)
      // Note that we have to keep track of the first character of the string for reasons that become apparent later
      val firstChar = initial.head
      val initialPairs = initial.zip(initial.tail).map { case (c1, c2) => s"$c1$c2" }

      // Initialise the map by feeding into it the starting set of strings
      val initialMap: CountingMap[String, BigInt] =
        initialPairs.foldLeft(CountingMap[String, BigInt]()) { case (map, pair) => map.add(pair)}

      // Run the transformation and take the 40th state
      val finalCounts = LazyList.unfold(initialMap)(substitute).drop(39).head

      // Note that the first letter of each pair is also the last letter of some pair. So to avoid overcounting, we need
      // to count the characters appearing as the _last_ character of each pair, and we need to add the very first
      // character of the string as an edge case (since this will never change, but will also never be accounted for as
      // the last character of a pair)
      val finalLetterMap = finalCounts.map.foldLeft(CountingMap[Char, BigInt]().add(firstChar, 1)) { case (charMap, (pair, count)) =>
        charMap.add(pair.last, count)
      }

      val min = finalLetterMap.map.minBy(_._2)._2
      val max = finalLetterMap.map.maxBy(_._2)._2

      (max - min)


    println("Test: " +  solve(testData))
    println("Actual: " + solve(data))

  /**
   * Parse the data to give the initial string and the sequence of substitutions.
   * @param value
   * @return
   */
  def parseData(value: LazyList[String]): (String, Seq[Substitution]) =
    val initial = value.head

    val substitutions = value.dropWhile(_.nonEmpty).drop(1).takeWhile(_.nonEmpty).map { s =>
      Substitution(s.charAt(0), s.charAt(1), s.charAt(6))
    }

    (initial, substitutions)

  /**
   * The substitution rule; when we encounter chars "f1" and "f2", we will want to insert char "to" between them
   * @param f1
   * @param f2
   * @param to
   */
  case class Substitution(f1: Char, f2: Char, to: Char) {

    // Map entry from (pair of adjacent characters) => char which should go between them
    def toTupleMapEntry: ((Char, Char), Char) = ((f1, f2) -> to)

    // Map entry from "string of two chars" => Seq of the two new 2-character strings created by insertion
    def toNewPairsMapEntry: (String, Seq[String]) = s"$f1$f2" -> Seq(s"$f1$to", s"$to$f2")
  }

}
