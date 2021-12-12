package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec

object Day12 {

  val testData1: LazyList[String] =
    """start-A
      |start-b
      |A-c
      |A-b
      |b-d
      |A-end
      |b-end""".stripMargin.splitAndTrimToList

  val testData2: LazyList[String] =
    """dc-end
      |HN-start
      |start-kj
      |dc-start
      |dc-HN
      |LN-dc
      |HN-end
      |kj-sa
      |kj-HN
      |kj-dc""".stripMargin.splitAndTrimToList

  val testData3: LazyList[String] =
    """fs-end
      |he-DX
      |fs-he
      |start-DX
      |pj-DX
      |end-zg
      |zg-sl
      |zg-pj
      |pj-he
      |RW-he
      |fs-DX
      |pj-RW
      |zg-RW
      |start-pj
      |he-WI
      |zg-he
      |pj-fs
      |start-RW""".stripMargin.splitAndTrimToList

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day12.txt").lines.filterNot(_.isEmpty)

  @main def d12p1(): Unit =
    def solve(seq: LazyList[String]): Unit =
      val caveStore = buildCaveStore(seq)
      val routes = countRoutes(caveStore)

      println(routes)

    solve(testData1)
//    solve(testData2)
//    solve(testData3)
    solve(data)

  @main def d12p2(): Unit =
    def solve(seq: LazyList[String]): Unit =
      val caveStore = buildCaveStore(seq)
      val routes = countRoutesWithBonusCave(caveStore)

      println(routes)

    solve(testData1)
//    solve(testData2)
//    solve(testData3)
    solve(data)


  def buildCaveStore(connections: LazyList[String]): CaveStore =
    def add(cur: Map[String, Set[String]], key: String, value: String) = cur.updatedWith(key) {
      case Some(existingSet) => Some(existingSet + value)
      case None => Some(Set(value))
    }
    @tailrec
    def buildMap(remaining: List[String], accum: Map[String, Set[String]]): Map[String, Set[String]] =
      remaining match {
        case head :: tail =>
          val bits = head.split("-")
          val c1 = bits(0)
          val c2 = bits(1)
          val addedC1ToC2 = add(accum, c1, c2)
          val addedC2ToC1 = add(addedC1ToC2, c2, c1)
          buildMap(tail, addedC2ToC1)
        case _ => accum
      }
    val caves = buildMap(connections.toList, Map.empty).map { case (name, connected) =>
      // Note - no point storing the link back to start, we should never use it.
      name match {
        case s if s == "start" => Start(connected.toSeq.sorted)
        case s if s == "end" => End
        case s if s.toUpperCase == s => Big(s, (connected - "start").toSeq.sorted)
        case s if s.toLowerCase == s => Small(s, (connected - "start").toSeq.sorted)
        case _ => throw new IllegalArgumentException(s"Bad cave name: '$name'")
      }
    }
    CaveStore(caves.toSeq)


  def countRoutes(implicit store: CaveStore): Int =
    def count(current: Cave, visited: List[Cave], outOfBounds: Set[Cave]): Int =
      val newVisited = current :: visited
      val remainingToVisit: Seq[Cave] = current.connections.filterNot(outOfBounds.contains)
      current match {
        case End =>
          1
        case s: Small =>
          remainingToVisit.map(r => count(r, newVisited, outOfBounds + s)).sum
        case _ =>
          remainingToVisit.map(r => count(r, newVisited, outOfBounds)).sum

      }


    count(store.get("start"), List.empty, Set.empty)

  val emptyCaveSet = Set.empty[List[Cave]]

  def countRoutesWithBonusCave(implicit store: CaveStore): Int =
    def getRoutes(current: Cave, visited: List[Cave], bonusCave: Option[Small], outOfBounds: Set[Cave]): Set[List[Cave]] =
      val newVisited = current :: visited
      val remainingToVisit: Seq[Cave] = current.connections.filterNot(outOfBounds.contains)
      current match {
        case End =>
          Set(newVisited)
        case s: Small =>
          bonusCave match {
            case Some(bonus) if bonus == s =>
              // Used up our bonus cave
              remainingToVisit.map(r => getRoutes(r, newVisited, None, outOfBounds)).fold(emptyCaveSet)(_ ++ _)
            case _ =>
              remainingToVisit.map(r => getRoutes(r, newVisited, bonusCave, outOfBounds + s)).fold(emptyCaveSet)(_ ++ _)
          }
        case _ =>
          remainingToVisit.map(r => getRoutes(r, newVisited, bonusCave, outOfBounds)).fold(emptyCaveSet)(_ ++ _)

      }

    val smallCaves = store.caves.collect { case s: Small => s}
    val uniqueRoutes: Set[List[Cave]] =
      smallCaves.map(bonus => getRoutes(store.get("start"), List.empty, Some(bonus), Set.empty)).fold(emptyCaveSet)(_ ++ _)
    uniqueRoutes.size



  case class CaveStore(caves: Seq[Cave]) {

    private val caveMap: Map[String, Cave] = caves.map(c => c.name -> c).toMap
    def get(name: String): Cave = caveMap(name)

    override def toString: String =
      caveMap.toSeq.sortBy(_._1).map { case (name, cave) => s"$name => " + cave.connectsTo.sorted.mkString("(",", ",")")}.mkString("\n")
  }

  sealed trait Cave {
    def name: String
    def connectsTo: Seq[String]
    def connections(implicit caveStore: CaveStore): Seq[Cave] = connectsTo.map(caveStore.get)
  }
  case class Start(connectsTo: Seq[String]) extends Cave {
    override val name: String = "start"
  }

  case object End extends Cave {
    override val name: String = "end"
    override val connectsTo: Seq[String] = Seq.empty
  }
  case class Big(name: String, connectsTo: Seq[String]) extends Cave {
    assert(name.toUpperCase == name)
  }

  case class Small(name: String, connectsTo: Seq[String]) extends Cave {
    assert(name.toLowerCase == name)
  }

}
