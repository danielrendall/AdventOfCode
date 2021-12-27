package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*
import uk.co.danielrendall.adventofcode.y2021.Day21.{DeterministicPlayerState, getInitialPositions}

import scala.annotation.tailrec
import scala.collection.SortedSet
import scala.util.matching.Regex

object Day22 {

  val testData: LazyList[String] =
    """on x=-20..26,y=-36..17,z=-47..7
      |on x=-20..33,y=-21..23,z=-26..28
      |on x=-22..28,y=-29..23,z=-38..16
      |on x=-46..7,y=-6..46,z=-50..-1
      |on x=-49..1,y=-3..46,z=-24..28
      |on x=2..47,y=-22..22,z=-23..27
      |on x=-27..23,y=-28..26,z=-21..29
      |on x=-39..5,y=-6..47,z=-3..44
      |on x=-30..21,y=-8..43,z=-13..34
      |on x=-22..26,y=-27..20,z=-29..19
      |off x=-48..-32,y=26..41,z=-47..-37
      |on x=-12..35,y=6..50,z=-50..-2
      |off x=-48..-32,y=-32..-16,z=-15..-5
      |on x=-18..26,y=-33..15,z=-7..46
      |off x=-40..-22,y=-38..-28,z=23..41
      |on x=-16..35,y=-41..10,z=-47..6
      |off x=-32..-23,y=11..30,z=-14..3
      |on x=-49..-5,y=-3..45,z=-29..18
      |off x=18..30,y=-20..-8,z=-3..13
      |on x=-41..9,y=-7..43,z=-33..15
      |on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
      |on x=967..23432,y=45373..81175,z=27513..53682""".stripMargin.splitAndTrimToList

  val testData2: LazyList[String] =
    """on x=-5..47,y=-31..22,z=-19..33
      |on x=-44..5,y=-27..21,z=-14..35
      |on x=-49..-1,y=-11..42,z=-10..38
      |on x=-20..34,y=-40..6,z=-44..1
      |off x=26..39,y=40..50,z=-2..11
      |on x=-41..5,y=-41..6,z=-36..8
      |off x=-43..-33,y=-45..-28,z=7..25
      |on x=-33..15,y=-32..19,z=-34..11
      |off x=35..47,y=-46..-34,z=-11..5
      |on x=-14..36,y=-6..44,z=-16..29
      |on x=-57795..-6158,y=29564..72030,z=20435..90618
      |on x=36731..105352,y=-21140..28532,z=16094..90401
      |on x=30999..107136,y=-53464..15513,z=8553..71215
      |on x=13528..83982,y=-99403..-27377,z=-24141..23996
      |on x=-72682..-12347,y=18159..111354,z=7391..80950
      |on x=-1060..80757,y=-65301..-20884,z=-103788..-16709
      |on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856
      |on x=-52752..22273,y=-49450..9096,z=54442..119054
      |on x=-29982..40483,y=-108474..-28371,z=-24328..38471
      |on x=-4958..62750,y=40422..118853,z=-7672..65583
      |on x=55694..108686,y=-43367..46958,z=-26781..48729
      |on x=-98497..-18186,y=-63569..3412,z=1232..88485
      |on x=-726..56291,y=-62629..13224,z=18033..85226
      |on x=-110886..-34664,y=-81338..-8658,z=8914..63723
      |on x=-55829..24974,y=-16897..54165,z=-121762..-28058
      |on x=-65152..-11147,y=22489..91432,z=-58782..1780
      |on x=-120100..-32970,y=-46592..27473,z=-11695..61039
      |on x=-18631..37533,y=-124565..-50804,z=-35667..28308
      |on x=-57817..18248,y=49321..117703,z=5745..55881
      |on x=14781..98692,y=-1341..70827,z=15753..70151
      |on x=-34419..55919,y=-19626..40991,z=39015..114138
      |on x=-60785..11593,y=-56135..2999,z=-95368..-26915
      |on x=-32178..58085,y=17647..101866,z=-91405..-8878
      |on x=-53655..12091,y=50097..105568,z=-75335..-4862
      |on x=-111166..-40997,y=-71714..2688,z=5609..50954
      |on x=-16602..70118,y=-98693..-44401,z=5197..76897
      |on x=16383..101554,y=4615..83635,z=-44907..18747
      |off x=-95822..-15171,y=-19987..48940,z=10804..104439
      |on x=-89813..-14614,y=16069..88491,z=-3297..45228
      |on x=41075..99376,y=-20427..49978,z=-52012..13762
      |on x=-21330..50085,y=-17944..62733,z=-112280..-30197
      |on x=-16478..35915,y=36008..118594,z=-7885..47086
      |off x=-98156..-27851,y=-49952..43171,z=-99005..-8456
      |off x=2032..69770,y=-71013..4824,z=7471..94418
      |on x=43670..120875,y=-42068..12382,z=-24787..38892
      |off x=37514..111226,y=-45862..25743,z=-16714..54663
      |off x=25699..97951,y=-30668..59918,z=-15349..69697
      |off x=-44271..17935,y=-9516..60759,z=49131..112598
      |on x=-61695..-5813,y=40978..94975,z=8655..80240
      |off x=-101086..-9439,y=-7088..67543,z=33935..83858
      |off x=18020..114017,y=-48931..32606,z=21474..89843
      |off x=-77139..10506,y=-89994..-18797,z=-80..59318
      |off x=8476..79288,y=-75520..11602,z=-96624..-24783
      |on x=-47488..-1262,y=24338..100707,z=16292..72967
      |off x=-84341..13987,y=2429..92914,z=-90671..-1318
      |off x=-37810..49457,y=-71013..-7894,z=-105357..-13188
      |off x=-27365..46395,y=31009..98017,z=15428..76570
      |off x=-70369..-16548,y=22648..78696,z=-1892..86821
      |on x=-53470..21291,y=-120233..-33476,z=-44150..38147
      |off x=-93533..-4276,y=-16170..68771,z=-104985..-24507""".stripMargin.splitAndTrimToList

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day22.txt").lines

  @main def d22p1() =
    def solve(seq: Seq[String]): Unit =
      val cubeSide: Interval = Interval(-50, 51)
      val instructions: Seq[Instruction] = parse(seq).zipWithIndex.map { case (pi, idx) => Instruction(pi, idx) }.flatMap(_.clipToCube(cubeSide))

      val set = instructions.foldLeft(Set.empty[Point]) { case (set, instruction) =>
        instruction.t match {
          case On => set ++ instruction.cuboid.points
          case Off => set -- instruction.cuboid.points
        }
      }
      println(set.size)

    solve(testData)
    solve(data)

  @main def d22p2() =
    def solve(seq: Seq[String]): Unit =
      val cubeSide: Interval = Interval(-50, 51)
      val instructions: Seq[Instruction] = parse(seq).zipWithIndex.map { case (pi, idx) => Instruction(pi, idx) }
      println(s"There are ${instructions.size} instructions")

      // Remaining = all instructions yet unprocessed
      // Existingnonintersecting = all of the cuboids we have so far which are all mutually non-intersecting
      @tailrec
      def getAllNonIntersectingInstructions(remaining: List[Instruction],
                                            existingNonIntersecting: Set[Instruction]): Set[Instruction] = {
        remaining match {
          case head :: tail =>
            println(s"Processing $head" + " with " + remaining.size + " remaining and " + existingNonIntersecting.size + " existing non intersecting")
            val updatedNonIntersecting = existingNonIntersecting.map { existing => head.overlaps(existing) }.flatMap {
              case NoInstructionOverlap(other) => Set(other)
              case InstructionCompletelyContained(_) => Set.empty
              case PartialInstructionOverlap(other, _, inOther) => inOther.map(c => Instruction(c, other.t, other.priority))
            }
            getAllNonIntersectingInstructions(tail, updatedNonIntersecting + head)

          case _ =>
            existingNonIntersecting
        }
      }

      val result = getAllNonIntersectingInstructions(instructions.toList, Set.empty)

      val total = result.filter(_.t == On).map(_.cuboid.volume).sum

      println(total)


    solve(testData2)
    solve(data)


  def parse(seq: Seq[String]): Seq[ParsedInstruction] = seq.map(ParsedInstruction.apply)

  sealed trait InstructionType

  case object On extends InstructionType

  case object Off extends InstructionType


  // Min is inclusive, max is exclusive
  case class Cuboid(xInt: Interval, yInt: Interval, zInt: Interval) {

    def points: LazyList[Point] = for {
      x <- xInt.values
      y <- yInt.values
      z <- zInt.values
    } yield Point(x, y, z)


    def corner: Point = Point(xInt.min, yInt.min, zInt.min)

    def volume: BigInt = BigInt(xInt.span) * BigInt(yInt.span) * BigInt(zInt.span)

    def contains(point: Point): Boolean = xInt.contains(point.x) && yInt.contains(point.y) && zInt.contains(point.z)

    def contains(other: Cuboid): Boolean = xInt.contains(other.xInt) &&
      yInt.contains(other.yInt) &&
      zInt.contains(other.zInt)

    def noOverlap(other: Cuboid): Boolean = xInt.noOverlap(other.xInt) ||
      yInt.noOverlap(other.yInt) ||
      zInt.noOverlap(other.zInt)

    override def toString: String = s"($xInt, $yInt, $zInt)"

  }

  sealed trait InstructionOverlap {
    def other: Instruction
  }

  case class NoInstructionOverlap(other: Instruction) extends InstructionOverlap

  case class InstructionCompletelyContained(other: Instruction) extends InstructionOverlap

  case class PartialInstructionOverlap(other: Instruction, cuboidsInThisOrBoth: Set[Cuboid], cuboidsInOtherOnly: Set[Cuboid]) extends InstructionOverlap


  case class ParsedInstruction(xMin: Int, xMax: Int, yMin: Int, yMax: Int, zMin: Int, zMax: Int, t: InstructionType) {
    assert(xMax >= xMin)
    assert(yMax >= yMin)
    assert(zMax >= zMin)
  }

  object ParsedInstruction {

    val LineRegex: Regex =
      "(on|off) x=(-?[0-9]+)..(-?[0-9]+),y=(-?[0-9]+)..(-?[0-9]+),z=(-?[0-9]+)..(-?[0-9]+)".r("typ", "xMin", "xMax", "yMin", "yMax", "zMin", "zMax")

    def apply(s: String): ParsedInstruction = s match {
      case LineRegex(t, xMin, xMax, yMin, yMax, zMin, zMax) =>
        ParsedInstruction(xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt, zMin.toInt, zMax.toInt, if (t == "on") On else if (t == "off") Off else throw new Exception("Bad line: " + s))
      case _ =>
        throw new Exception("Bad line: " + s)
    }
  }

  case class Instruction(cuboid: Cuboid, t: InstructionType, priority: Int) {

    def overlaps(other: Instruction): InstructionOverlap =

      if (cuboid.contains(other.cuboid)) {
        InstructionCompletelyContained(other)
      } else if (cuboid.noOverlap(other.cuboid)) {
        NoInstructionOverlap(other)
      } else {

        @tailrec
        def triage(remainingCuboids: List[Cuboid], inThisOrBoth: Set[Cuboid], inOtherOnly: Set[Cuboid]): PartialInstructionOverlap =
          remainingCuboids match {
            case next :: rest =>
              if (cuboid.contains(next)) {
                triage(rest, inThisOrBoth + next, inOtherOnly)
              } else if (other.cuboid.contains(next)) {
                triage(rest, inThisOrBoth, inOtherOnly + next)
              } else {
                triage(rest, inThisOrBoth, inOtherOnly)
              }
            case _ =>
              PartialInstructionOverlap(other, inThisOrBoth, inOtherOnly)
          }

        val allCuboids: List[Cuboid] = (for {
          xInt <- cuboid.xInt.overlaps(other.cuboid.xInt)
          yInt <- cuboid.yInt.overlaps(other.cuboid.yInt)
          zInt <- cuboid.zInt.overlaps(other.cuboid.zInt)
        } yield Cuboid(xInt, yInt, zInt)).toList

        triage(allCuboids, Set.empty, Set.empty)
      }

    def clipToCube(cubeSide: Interval): Option[Instruction] =
      for {
        newXRange <- cuboid.xInt.clipTo(cubeSide)
        newYRange <- cuboid.yInt.clipTo(cubeSide)
        newZRange <- cuboid.zInt.clipTo(cubeSide)
      } yield Instruction(Cuboid(newXRange, newYRange, newZRange), t, priority)

  }

  object Instruction {

    def apply(p: ParsedInstruction, idx: Int): Instruction =
      Instruction(Cuboid(Interval(p.xMin, p.xMax + 1), Interval(p.yMin, p.yMax + 1), Interval(p.zMin, p.zMax + 1)), p.t, idx)
  }

  // min is inclusive, max is exclusive
  case class Interval(min: Int, max: Int) {
    assert(min <= max)

    // Compare with the given region and compute the intervals if there's an overlap. Note that if there's no overlap,
    // we return 0 intervals rather than 2 separate ones. It follows that if any overlaps are returned, they're
    // contiguous. We can return fewer than 3 overlaps if the starts or ends match
    // Options:
    // this.min - this.max - other.min - other.max = no overlap, this is first
    // this.min - other.min - this.max - other.max = overlap, this starts first
    // this.min - other.min - other.max - this.max = this contains other
    // other.min - this.min - this.max - other.max = other contains this
    // other.min - this.min - other.max - this.max = overlap, other starts first
    // other.min - other.max - this.min - this.max = no overlap, other is first
    def overlaps(o: Interval): Seq[Interval] =
      (if (min <= o.min) { // this.min is first
        if (max <= o.min) {
          Seq.empty // no overlap, this is first
        } else if (o.max <= max) {
          Seq(Interval(min, o.min), o, Interval(o.max, max)) // this contains other
        } else {
          Seq(Interval(min, o.min), Interval(o.min, max), Interval(max, o.max)) //overlap, this starts first
        }
      } else { // other.min is first
        if (o.max <= min) {
          Seq.empty // no overlap, other is first
        } else if (max <= o.max) {
          Seq(Interval(o.min, min), this, Interval(max, o.max)) // other contains this
        } else {
          Seq(Interval(o.min, min), Interval(min, o.max), Interval(o.max, max))
        }
      }).filter(_.span > 0)

    def clipTo(other: Interval): Option[Interval] = {
      if (max <= other.min || other.max <= min) {
        None
      } else {
        Some(Interval(Math.max(other.min, min), Math.min(other.max, max)))
      }
    }

    def span: Int = max - min

    def values: LazyList[Int] = (min until max).to(LazyList)

    def contains(value: Int): Boolean = (min <= value) && (value < max)

    def contains(other: Interval): Boolean = (min <= other.min) && (other.max <= max)

    def noOverlap(other: Interval): Boolean = (max <= other.min) || (other.max <= min)

    def endpoints: SortedSet[Int] = SortedSet(min, max)

    override def toString = s"$min->$max"
  }

  case class Point(x: Int, y: Int, z: Int)


}
