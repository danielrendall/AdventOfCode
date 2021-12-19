package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.CountingMap
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.MapView
import scala.util.matching.Regex

object Day19 {


  val testData: LazyList[String] =
    """--- scanner 0 ---
      |404,-588,-901
      |528,-643,409
      |-838,591,734
      |390,-675,-793
      |-537,-823,-458
      |-485,-357,347
      |-345,-311,381
      |-661,-816,-575
      |-876,649,763
      |-618,-824,-621
      |553,345,-567
      |474,580,667
      |-447,-329,318
      |-584,868,-557
      |544,-627,-890
      |564,392,-477
      |455,729,728
      |-892,524,684
      |-689,845,-530
      |423,-701,434
      |7,-33,-71
      |630,319,-379
      |443,580,662
      |-789,900,-551
      |459,-707,401
      |
      |--- scanner 1 ---
      |686,422,578
      |605,423,415
      |515,917,-361
      |-336,658,858
      |95,138,22
      |-476,619,847
      |-340,-569,-846
      |567,-361,727
      |-460,603,-452
      |669,-402,600
      |729,430,532
      |-500,-761,534
      |-322,571,750
      |-466,-666,-811
      |-429,-592,574
      |-355,545,-477
      |703,-491,-529
      |-328,-685,520
      |413,935,-424
      |-391,539,-444
      |586,-435,557
      |-364,-763,-893
      |807,-499,-711
      |755,-354,-619
      |553,889,-390
      |
      |--- scanner 2 ---
      |649,640,665
      |682,-795,504
      |-784,533,-524
      |-644,584,-595
      |-588,-843,648
      |-30,6,44
      |-674,560,763
      |500,723,-460
      |609,671,-379
      |-555,-800,653
      |-675,-892,-343
      |697,-426,-610
      |578,704,681
      |493,664,-388
      |-671,-858,530
      |-667,343,800
      |571,-461,-707
      |-138,-166,112
      |-889,563,-600
      |646,-828,498
      |640,759,510
      |-630,509,768
      |-681,-892,-333
      |673,-379,-804
      |-742,-814,-386
      |577,-820,562
      |
      |--- scanner 3 ---
      |-589,542,597
      |605,-692,669
      |-500,565,-823
      |-660,373,557
      |-458,-679,-417
      |-488,449,543
      |-626,468,-788
      |338,-750,-386
      |528,-832,-391
      |562,-778,733
      |-938,-730,414
      |543,643,-506
      |-524,371,-870
      |407,773,750
      |-104,29,83
      |378,-903,-323
      |-778,-728,485
      |426,699,580
      |-438,-605,-362
      |-469,-447,-387
      |509,732,623
      |647,635,-688
      |-868,-804,481
      |614,-800,639
      |595,780,-596
      |
      |--- scanner 4 ---
      |727,592,562
      |-293,-554,779
      |441,611,-461
      |-714,465,-776
      |-743,427,-804
      |-660,-479,-426
      |832,-632,460
      |927,-485,-438
      |408,393,-506
      |466,436,-512
      |110,16,151
      |-258,-428,682
      |-393,719,612
      |-211,-452,876
      |808,-476,-593
      |-575,615,604
      |-485,667,467
      |-680,325,-822
      |-627,-443,-432
      |872,-547,-609
      |833,512,582
      |807,604,487
      |839,-516,451
      |891,-625,532
      |-652,-548,-490
      |30,-46,-14""".stripMargin.splitAndTrimToList

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day19.txt").lines

  @main def d19p1() =
    def solve(seq: Seq[String]): Unit =
      val resolvedResults = getBeaconLocations(seq)
      val beacons = resolvedResults.flatten(_.allOffsetBeacons)
      println("Got " + beacons.size + " beacons including duplicates")
      val uniqueBeacons = beacons.groupBy(_.relPos).keys

      println("Got " + uniqueBeacons.size + " beacon groups")
    solve(testData)
    solve(data)

  @main def d19p2() =
    def solve(seq: Seq[String]): Unit =
      val resolvedResults = getBeaconLocations(seq)
      val zipped = resolvedResults.map(_.offset).zipWithIndex
      val allVectorPairs: Iterable[(Vector, Vector)] = for {
        (b1, i1) <- zipped
        (b2, i2) <- zipped
        if (i1 < i2)
      } yield (b1, b2)
      val all = allVectorPairs.map { case (v1, v2) => (v1, v2, (v2 - v1).manhattanLength)}.toList.sortBy(_._3)(Ordering.Int.reverse)
      println(all.head)
    solve(testData)
    solve(data)

  // All relative to the same (arbitrary) origin
  def getBeaconLocations(seq: Seq[String]): Iterable[ResolvedResult] =
    val scannerResults = parseData(seq)
      println("Got " + scannerResults.size + " results")
      scannerResults match {
        case reference :: rest =>
          gatherScanners(List(ResolvedResult(Vector.zero, reference)), rest)
        case _ => throw new Exception("No scanners!")
      }


  @tailrec
  def gatherScanners(resolvedResults: List[ResolvedResult], remaining: List[ScannerResult]): List[ResolvedResult] =
    remaining match {
      case next :: rest =>
        resolve(next, resolvedResults) match {
          case Some(newResolvedResult: ResolvedResult) =>
            gatherScanners(newResolvedResult :: resolvedResults, rest)
          case _ =>
            // Didn't find, so put it to the back of the list and try the next one
//            println(s"Didn't resolve ${next.number} with ${resolvedResults.size} already done")
            gatherScanners(resolvedResults, rest :+ next)
        }

      case _ => resolvedResults
    }

  // Try to resolve against a new scanner result against one of the previous results (we don't care which, we assume it
  // will all turn out all right)
  def resolve(newResult: ScannerResult, resolvedResults: List[ResolvedResult]): Option[ResolvedResult] =
    resolvedResults.to(LazyList).flatMap(rr => resolve(newResult, rr)).headOption

  // Try to resolve a new scanner result against a specific previous result i.e. find an offset and rotation that can
  // be applied to the scannerResult
  def resolve(newResult: ScannerResult, resolvedResult: ResolvedResult): Option[ResolvedResult] =
//    println("Resolving " + newResult.number + " compared to " + resolvedResult.scannerResult.number)
    def find(remainingNorms: List[Int], alreadyTried: CountingMap[OffsetAndRotation, Int]): Option[ResolvedResult] =
      remainingNorms match {
        case current :: rest =>
          val resolvedPairsForNorm = resolvedResult.scannerResult.normMap(current)
          val potentialNewPairsForNorm = newResult.normMap(current)
          val options = for {
            existingPair <- resolvedPairsForNorm
            newPair <- potentialNewPairsForNorm
          } yield resolve(existingPair, newPair)
          val newCounts = options.foldLeft(alreadyTried) { case (accum, mapToAdd) => accum.addAll(mapToAdd) }
          find(rest, newCounts)
        case _ =>
          alreadyTried.sortedSizeFirst.headOption match {
            case Some((offsetAndRotation, count)) =>
              if (count >= 12) {
                // We want to add the offset relative to the existing resolved result to whatever offset _that_ had
                Some(ResolvedResult(resolvedResult.offset + offsetAndRotation.offset, newResult.transformed(offsetAndRotation.rotation)))
              } else {
//                println("Not convinced: " + count + " votes for " + offsetAndRotation)
                None
              }
            case _ => None
          }
      }


    find(newResult.sortedNorms.intersect(resolvedResult.scannerResult.sortedNorms).toList, CountingMap())


  def resolve(existingPair: (Beacon, Beacon), newPair: (Beacon, Beacon)): CountingMap[OffsetAndRotation, Int] = {
    // Can we apply a transformation to the new pair so that they can be aligned with the target vector (either way round)?
    // We need to apply the transformation to each beacon in pair and then check afterwards, because we'll want to know
    // where in space they end up for calculating the offset...
    val targetVector = existingPair._2.relPos - existingPair._1.relPos

    val offsetAndRotations = Matrix.matrixes.flatMap { m =>
      val newStart = m * newPair._1.relPos
      val newEnd = m * newPair._2.relPos

      val testVector = newEnd - newStart
      if (testVector == targetVector) {
        // the old start corresponds to the new start; how much have we have to offset the new system?
        // Relative to reference origin, vector has position existingPair.start
        // Relative to originin the new sust, vector has position newStart
        // Suppose newStart = 0; then displacement of new system would be existingPair.start
        // Suppose old start = 0, then displacement of new system would be -newStart
        // So general displacement is existingpair.start - newstart
        val offset = existingPair._1.relPos - newStart
        Some(OffsetAndRotation(offset, m))
      } else if (-testVector == targetVector) {
        // Same but the other way round... newEnd corresponds to old start
        val offset = existingPair._1.relPos - newEnd
        Some(OffsetAndRotation(offset, m))
      } else {
        None
      }
    }
    CountingMap().add(offsetAndRotations)

  }


  case class OffsetAndRotation(offset: Vector, rotation: Matrix)


  case class Vector(x: Int, y: Int, z: Int) {
    lazy val norm: Int = x * x + y * y + z * z

    def +(o: Vector): Vector = Vector(x + o.x, y + o.y, z + o.z)

    def -(o: Vector): Vector = Vector(x - o.x, y - o.y, z - o.z)

    def unary_- : Vector = Vector(-x, -y, -z)

    def manhattanLength: Int = Math.abs(x) + Math.abs(y) + Math.abs(z)
  }

  object Vector {
    val i: Vector = Vector(1, 0, 0)
    val j: Vector = Vector(0, 1, 0)
    val k: Vector = Vector(0, 0, 1)
    val zero: Vector = Vector(0, 0, 0)
  }

  // Represents a beacon, relative to some origin
  // ID uniquely identifies the beacon, even if we rotate all of the beacons for that origin
  case class Beacon(id: UUID, relPos: Vector)

  // A result from a scanner
  case class ScannerResult(number: Int, beacons: List[Beacon]) {
    val zipped: Seq[(Beacon, Int)] = beacons.zipWithIndex
    lazy val normMap: Map[Int, Set[(Beacon, Beacon)]] = (for {
      (b1, i1) <- zipped
      (b2, i2) <- zipped
      if (i2 < i1)
    } yield (b1.relPos - b2.relPos).norm -> (b1, b2)).groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap

    // All norms in reverse order...
    lazy val sortedNorms: Seq[Int] = normMap.keySet.toSeq.sorted(Ordering.Int.reverse)

    def transformed(matrix: Matrix): ScannerResult =
      copy(beacons = beacons.map(b => b.copy(relPos = matrix * b.relPos)))
  }

  // A scanner result (which needs to have had its orientation fixed) and its offset from the first result which we'll
  // take as being the origin.
  case class ResolvedResult(offset: Vector, scannerResult: ScannerResult) {
    def allOffsetBeacons: Seq[Beacon] = scannerResult.beacons.map(b => b.copy(relPos = b.relPos + offset))
  }

  val scannerRegex: Regex = "--- scanner ([0-9]+) ---".r("num")
  val beaconRegex: Regex = "(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)".r("num")

  def parseData(seq: Seq[String]): List[ScannerResult] =

    @tailrec
    def parse(remaining: List[String],
              scannerNumberOpt: Option[Int],
              beacons: List[Beacon],
              accum: List[ScannerResult]): List[ScannerResult] =
      remaining match {
        case head :: rest =>
          scannerNumberOpt match {
            case Some(scannerNumber) =>
              if (head.trim.isEmpty) {
                parse(rest, None, List.empty, ScannerResult(scannerNumber, beacons.reverse) :: accum)
              } else {
                head match {
                  case beaconRegex(a, b, c) =>
                    parse(rest, scannerNumberOpt, Beacon(UUID.randomUUID(), Vector(a.toInt, b.toInt, c.toInt)) :: beacons, accum)
                  case _ => throw new Exception(s"Bad beacon line: '$head'")
                }
              }
            case _ =>
              head match {
                case scannerRegex(num) => parse(rest, Some(num.toInt), List.empty, accum)
                case _ => throw new Exception(s"Bad scanner line: '$head'")
              }
          }
        case _ =>
          scannerNumberOpt match {
            case Some(scannerNumber) =>
              (ScannerResult(scannerNumber, beacons.reverse) :: accum).reverse
            case _ =>
              accum.reverse
          }
      }

    parse(seq.toList, None, List.empty, List.empty)

  case class Matrix(c1: Vector, c2: Vector, c3: Vector) {

    override def toString: String =
      """% 2d, % 2d, % 2d
        |% 2d, % 2d, % 2d
        |% 2d, % 2d, % 2d""".stripMargin.format(c1.x, c2.x, c3.x, c1.y, c2.y, c3.y, c1.z, c2.z, c3.z)

    def *(v: Vector): Vector =
      Vector(c1.x * v.x + c2.x * v.y + c3.x * v.z,
        c1.y * v.x + c2.y * v.y + c3.y * v.z,
        c1.z * v.x + c2.z * v.y + c3.z * v.z)


    def *(m: Matrix): Matrix =
      Matrix(this * m.c1, this * m.c2, this * m.c3)

    lazy val det: Int =
      c1.x * (c2.y * c3.z - c2.z * c3.y) +
      c2.x * (c3.y * c1.z - c3.z * c1.y) +
      c3.x * (c1.y * c2.z - c1.z * c2.y)
  }

  object Matrix {
    import Vector._

    // All 24 matrices in the octahedral group = take all possible combinations and choose the ones with determinant 1
    val matrixes: Seq[Matrix] = (for {
      c1 <- Seq(i, -i)
      c2 <- Seq(j, -j)
      c3 <- Seq(k, -k)
    } yield {
      val columns = Seq(c1, c2, c3)
      for {
        x <- columns
        y <- columns.filterNot(_ == x)
        z <- columns.filterNot(s => s == x || s == y)
      } yield Matrix(x, y, z)
    }).flatten.filter(_.det == 1)

    // They're all different
    assert(matrixes.size == matrixes.toSet.size)

    val identity: Matrix = matrixes.head

  }


}
