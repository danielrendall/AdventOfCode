package uk.co.danielrendall.adventofcode.y2015

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.util.matching.Regex

object Day9 {

  val testData: LazyList[String] =
    """London to Dublin = 464
      |London to Belfast = 518
      |Dublin to Belfast = 141""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2015/day9.txt").lines

  def computeAllPossibleDistances(routes: Seq[Route]): Seq[Int] =
    val places = routes.flatMap(s => Seq(s.start, s.end)).distinct.sorted
    val lookup: Map[(String, String), Int] = routes.flatMap(r => Seq((r.start, r.end) -> r.distance, (r.end, r.start) -> r.distance)).toMap
    listOfRoutes(places.toList).map { ordering =>
      ordering.zip(ordering.tail).map(s => lookup(s)).sum
    }

  def listOfRoutes(places: List[String]): List[List[String]] =
    if (places.tail.isEmpty) List(List(places.head)) else
      for {
      place <- places
      rest = places.filterNot(_ == place)
      result <- listOfRoutes(rest).map(list => (place :: list))
    } yield result

  @main def d9p1(): Unit = {
    def solve(list: LazyList[String]) =
      val routes = list.map(Route.apply).toList
      computeAllPossibleDistances(routes).min

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d9p2(): Unit = {
    def solve(list: LazyList[String]) =
      val routes = list.map(Route.apply).toList
      computeAllPossibleDistances(routes).max

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  case class Route(start: String, end: String, distance: Int)

  object Route {

    private val regex: Regex = "^([A-Za-z]+) to ([A-Za-z]+) = ([0-9]+)".r

    def apply(s: String): Route = s match
      case regex(start, end, distance) => Route(start, end, distance.toInt)


  }
}

