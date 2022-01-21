package uk.co.danielrendall.adventofcode.utils

import org.specs2.mutable.Specification

class CountingMapSpec extends Specification {

  "CountingMap.add one" should {
    "Add to an empty map" in {
      val map = CountingMap[String, Int]().add("Hello")
      map.map.get("Hello") must beSome(1)
      map.map.get("Goodbye") must beNone
    }

    "Add to an existing map" in {
      val map = CountingMap[String, Int]().add("Hello")
      map.map.get("Hello") must beSome(1)
      val map2 = map.add("Hello")
      map2.map.get("Hello") must beSome(2)
    }

    "Accept a BigInt" in {
      val map = CountingMap[String, BigInt]().add("Hello")
      map.map.get("Hello") must beSome(BigInt(1))
      map.map.get("Goodbye") must beNone
    }

  }

}
