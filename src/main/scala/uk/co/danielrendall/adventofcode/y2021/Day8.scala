package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day8 {

  val testData: LazyList[String] =
    """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
      |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
      |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
      |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
      |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
      |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
      |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
      |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
      |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
      |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce""".stripMargin.splitAndTrimToList


  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day8.txt").lines.filterNot(_.isEmpty)

  @main def d8p1() =
    def solve(seq: LazyList[String]): Unit =
      val decoded: Seq[Seq[Char]] = seq.map(decode)
      val count = decoded.flatten.count(s => s == '1' || s == '4' || s == '7' || s == '8')
      println(count)


    solve(testData)
    solve(data)

  @main def d8p2() =
    def solve(seq: LazyList[String]): Unit =
      val decoded: Seq[Seq[Char]] = seq.map(decode)
      val sum = decoded.map(_.mkString.toInt).sum
      println(sum)


    solve(testData)
    solve(data)

  def decode(str: String): Seq[Char] =
    val halves = str.split('|').map(_.trim)
    val patterns: Set[String] = halves(0).split(' ').toSet
    val digitPatterns: Seq[Set[Char]] = halves(1).split(' ').toSeq.map(_.toSet)
    val lookup = workOutDigits(patterns)
    digitPatterns.map(lookup.apply)


  // Segment names: t, tl, tr, m, bl, br, b

  def workOutDigits(patterns: Set[String]): Map[Set[Char], Char] =
    // One has 2 segments
    val (oneSet, oneRest) = patterns.partition(_.length == 2)
    // Seven has 3 segments
    val (sevenSet, sevenRest) = oneRest.partition(_.length == 3)
    // Four has 4 segments
    val (fourSet, fourRest) = sevenRest.partition(_.length == 4)
    // Eight has 7 segments
    val (eightSet, eightRest) = fourRest.partition(_.length == 7)
    // Remaining: Two, Three, Five (5 segments each), Zero, Six, Nine (6 segments each)
    val (twoThreeFive, zeroSixNine) = eightRest.partition(_.length == 5)

    // These will all work...
    val one: String = oneSet.head
    val seven: String = sevenSet.head
    val four: String = fourSet.head
    val eight: String = eightSet.head

    val oneSegmentSet = one.toSet
    val sevenSegmentSet = seven.toSet
    val fourSegmentSet = four.toSet
    val eightSegmentSet = eight.toSet
    // Top segment appears in Seven but not One
    val t: Char = (seven.toSet -- oneSegmentSet).head

    // Consider the zeroSixNine set; if we remove the top segment, one of these will have the same segments as Four but
    // with an additional one, which gives us Nine and the bottom segment. The other two are missing at least one of the
    // segments of Four.
    val (nine: String, b: Char) = {
      // Map of (set of segments minus t -> number)
      val zeroSixNineMinusT: Map[Set[Char], String] = zeroSixNine.map(s => (s.toSet - t) -> s).toMap
      val key = zeroSixNineMinusT.keySet.find(s => fourSegmentSet.forall(x => s.contains(x))).get
      val nine = zeroSixNineMinusT(key)
      val b = ((nine.toSet - t) -- fourSegmentSet).head
      (nine, b)
    }
    val zeroSix: Set[String] = zeroSixNine - nine

    // Consider twoThreeFive set; if we remove the top and bottom segments, only one of these will have the same segments
    // as One with an additional one, which gives us Three and the middle segment
    val (three: String, m: Char) = {
      val twoThreeFiveMinusTandB: Map[Set[Char], String] = twoThreeFive.map(s => ((s.toSet - t) - b) -> s).toMap
      val key = twoThreeFiveMinusTandB.keySet.find(s => oneSegmentSet.forall(x => s.contains(x))).get
      val three = twoThreeFiveMinusTandB(key)
      val m = ((three.toSet - t - b) -- oneSegmentSet).head
      (three, m)
    }

    val twoFive: Set[String] = twoThreeFive - three

    // Which of zeroSix contains the middle segment?
    val (sixSet, zeroSet) = zeroSix.partition(_.contains(m))

    val six = sixSet.head
    val zero = zeroSet.head

    // If we get the tr segment we can tell Two and Five apart
    val tr = (eightSegmentSet -- six.toSet).head

    val (twoSet, fiveSet) = twoFive.partition(_.contains(tr))

    val two = twoSet.head
    val five = fiveSet.head

    Map(zero.toSet -> '0', oneSegmentSet -> '1', two.toSet -> '2', three.toSet -> '3', fourSegmentSet -> '4',
      five.toSet -> '5', six.toSet -> '6', sevenSegmentSet -> '7', eightSegmentSet -> '8', nine.toSet -> '9')


}
