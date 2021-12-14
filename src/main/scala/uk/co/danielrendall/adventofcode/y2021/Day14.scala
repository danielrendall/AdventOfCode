package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.CountingMap
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*
import uk.co.danielrendall.adventofcode.y2021.Day13.{data, parseData, performFold, testData}

object Day14 {

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
    def solve(seq: LazyList[String]): Unit =
      val (initial, substitutions) = parseData(seq)
      val substitutionMap = substitutions.map(_.toTupleMapEntry).toMap

      def substitute(current: String): Option[(String, String)] = {
        val withSpace = current + " "
        val result = withSpace.zip(withSpace.tail).map { pair =>
          substitutionMap.get(pair) match {
            case Some(newChar) => s"${pair._1}$newChar"
            case None => s"${pair._1}"
          }
        }.mkString.trim
        Some((result, result))
      }

      val finalState = LazyList.unfold(initial)(substitute).drop(9).head
      println(finalState.length)
      println(finalState)
      val stats = lettersAndCounts(finalState)
      val min = stats.minBy(_._2)._2
      val max = stats.maxBy(_._2)._2
      println(max - min)

    solve(testData)
    solve(data)

  @main def d14p2() =
    def solve(seq: LazyList[String]): Unit =
      val (initial, substitutions) = parseData(seq)
      val substitutionMap: Map[String, Seq[String]] = substitutions.map(_.toNewPairsMapEntry).toMap

      def substitute(current: CountingMap[String, BigInt]): Option[(CountingMap[String, BigInt], CountingMap[String, BigInt])] = {
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

      val firstChar = initial.head
      val initialPairs = initial.zip(initial.tail).map { case (c1, c2) => s"$c1$c2" }

      val initialMap: CountingMap[String, BigInt] =
        initialPairs.foldLeft(CountingMap[String, BigInt]()) { case (map, pair) => map.add(pair)}

      val finalCounts = LazyList.unfold(initialMap)(substitute).drop(39).head

      val finalLetterMap = finalCounts.map.foldLeft(CountingMap[Char, BigInt]().add(firstChar, 1)) { case (charMap, (pair, count)) =>
        charMap.add(pair.last, count)
      }

      val min = finalLetterMap.map.minBy(_._2)._2
      val max = finalLetterMap.map.maxBy(_._2)._2

      println(max - min)


    solve(testData)
    solve(data)

  def parseData(value: LazyList[String]): (String, Seq[Substitution]) =
    val initial = value.head

    val substitutions = value.dropWhile(_.nonEmpty).drop(1).takeWhile(_.nonEmpty).map { s =>
      Substitution(s.charAt(0), s.charAt(1), s.charAt(6))
    }

    (initial, substitutions)

  case class Substitution(f1: Char, f2: Char, to: Char) {
    def toTupleMapEntry: ((Char, Char), Char) = ((f1, f2) -> to)

    def toNewPairsMapEntry: (String, Seq[String]) = s"$f1$f2" -> Seq(s"$f1$to", s"$to$f2")
  }

  def lettersAndCounts(string: String): Set[(Char, Int)] =
    string.toSet.map(test => (test, string.count(c => c == test)))
}
