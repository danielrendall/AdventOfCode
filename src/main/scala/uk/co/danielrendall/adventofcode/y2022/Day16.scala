package uk.co.danielrendall.adventofcode.y2022

import uk.co.danielrendall.adventofcode.utils.FifoQueue
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

object Day16 {

  val testData: LazyList[String] =
    """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
      |Valve BB has flow rate=13; tunnels lead to valves CC, AA
      |Valve CC has flow rate=2; tunnels lead to valves DD, BB
      |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
      |Valve EE has flow rate=3; tunnels lead to valves FF, DD
      |Valve FF has flow rate=0; tunnels lead to valves EE, GG
      |Valve GG has flow rate=0; tunnels lead to valves FF, HH
      |Valve HH has flow rate=22; tunnel leads to valve GG
      |Valve II has flow rate=0; tunnels lead to valves AA, JJ
      |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin.splitAndTrimToList

  val games: LazyList[String] = this.getClass.getResourceAsStream("/2022/day16.txt").lines.filterNot(_.isEmpty)

  @main def d16p1(): Unit =
    def solve(list: LazyList[String]) =
      val valves: Seq[ParsedValve] = list.map(parse)
      val network = new Network(valves, "AA")

      network.allNodes.sortBy(_.id).foreach { n =>
        println(n.id + ": " + n.canReach.toList.sortBy(_._2).map(x => (x._2 + "->" + x._1)).mkString(", "))
      }

      val maxTime = 30

      case class State(time: Int,
                       currentNode: network.Node,
                       open: Set[network.Node],
                       totalSoFar: Int) extends Ordered[State] {



        def reachableUnopened = currentNode.canReach

        def remaining: Int = (network.allNodes.toSet -- open).map(n => n.rate * (maxTime - time)).sum

        def isOpen(node: network.Node): Boolean = open.contains(node)

        override def toString(): String = "Total: " + totalSoFar + " Open: " + path.toString() + " Time: " + time

        override def compare(that: State): Int = {
          // If this is negative, our total is higher, so we come first
          val totalDifference = that.totalSoFar - totalSoFar
          if (totalDifference != 0) {
            totalDifference
          } else {
            // If that.time is greater, this is negative - we have the same total, but we have more time left
            time - that.time
          }

        }
      }

      val init = State(0, List(network.startNode), Set.empty, 0)

      val bestSoFar = new AtomicReference[State](init)

      def search(statesToSearch: SortedSet[State]): Unit = {
        import math.Ordered.orderingToOrdered
        //        if (currentState.canCatch(bestSoFar.get.totalSoFar)) {
        println("Searching " + currentState)
        if (currentState.totalSoFar > bestSoFar.get().totalSoFar) {
          bestSoFar.set(currentState)
        }
        val currentNode = currentState.path.head
        currentNode.canReach
          .filterNot { case (node, time) =>
            currentState.open.contains(node)
          }
          .foreach { case (node, time) =>
            val timeOnArrival = time + currentState.time

            if (currentNode.rate > 0 && !currentState.isOpen(currentNode) && (timeOnArrival + 1) < maxTime) {
              search(State(timeOnArrival + 1, node :: currentState.path, currentState.open + currentNode,
                currentState.totalSoFar + currentNode.rate * (maxTime - currentState.time + 1)))
            }

            if (timeOnArrival < maxTime) {
              search(State(timeOnArrival, node :: currentState.path, currentState.open, currentState.totalSoFar))
            }
          }
      }

      search(init)
      bestSoFar.get()


    println("Test: " + solve(testData))
  //    println("Actual: " + solve(games))

  @main def d16p2(): Unit =
    def solve(list: LazyList[String]) = ???

    println("Test: " + solve(testData))
    println("Actual: " + solve(games))

  // These are what we get from the data
  case class ParsedValve(id: String, rate: Int, tunnels: Set[String])

  private val ValveRegex: Regex = "Valve ([A-Z]+) has flow rate=(\\d+); tunnels? leads? to valves? ([A-Z, ]+)".r

  private def parse(line: String) = line match
    case ValveRegex(id, r, others) => ParsedValve(id, r.toInt, others.split(',').map(_.trim).toSet)
    case _ => throw new IllegalArgumentException("Bad line: " + line)

  case class Node(id: String, rate: Int)

  case class Network(map: Map[Node, Map[Node, Int]])

  object Network {

    def apply(parsedValves: Seq[ParsedValve], startNodeId: String): Network = {

      // allNodes = all of the nodes in the system
      val allNodes: Seq[Node] = parsedValves.map(pv => Node(pv.id, pv.rate))

      // Map from a node to each of its immediate neighbours
      val nodeToNeighboursMap: Map[Node, Set[Node]] = {
        val map: Map[String, Node] = allNodes.foldLeft(Map[String, Node]()) { (m, n) => m + (n.id -> n) }
        parsedValves.map { pv =>
          val node = map(pv.id)
          val neighbours = pv.tunnels.map(t => map(t)).toSet
          node -> neighbours
        }.toMap
      }

      val temporaryNodeMap: Map[Node, Map[Node, Int]] = allNodes.map(node => node -> computeFastestRoutes(node, nodeToNeighboursMap)).toMap

      val startNode = allNodes.find(_.id == startNodeId).getOrElse(throw new IllegalStateException("No start node"))

      val allImportantNodes: Set[Node] =
        allNodes.filter(_.rate > 0).toSet + startNode
      val filteredMap = m.filter { case (id, node) => allImportantNodes.contains(node) }
      allImportantNodes.foreach(n => n.keepOnlyImportant(allImportantNodes))
      (filteredMap, startNode)

    }

    private def computeFastestRoutes(origin: Node, nodeToNeighboursMap: Map[Node, Set[Node]]): Map[Node, Int] = {

      // Map of all nodes (other than the origin), assuming a ridiculously big distance to each one
      val worstPossibleMap: Map[Node, Int] = (nodeToNeighboursMap.keySet - origin).map(n => n -> Integer.MAX_VALUE).toMap

      @tailrec
      def recurse(toCheck: Set[Node], currentBestMap: Map[Node, Int]): Map[Node, Int] = {
        if (toCheck.nonEmpty) {


          val newToCheck: Set[Node] = toCheck.flatMap { n =>
            val bestStepsToN: Int = _canReach.getOrElse(n, throw new IllegalStateException(s"Should have had best path to ${n.id}"))
            n._neighbours.flatMap { neighbour =>
              val currentBestStepsToNeighbour = _canReach.getOrElse(neighbour, Integer.MAX_VALUE)
              val bestViaN = bestStepsToN + 1
              if (bestViaN < currentBestStepsToNeighbour) {
                _canReach.put(neighbour, bestViaN)
                Some(neighbour)
              } else None

            }
          }
          recurse(newToCheck)
        } else{
          currentBestMap
        }
      }

      recurse(Set(origin), worstPossibleMap)

    }

  }


}
