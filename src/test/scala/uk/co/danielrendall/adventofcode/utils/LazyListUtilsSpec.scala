package uk.co.danielrendall.adventofcode.utils
import LazyListUtils._

import org.specs2.mutable.Specification

class LazyListUtilsSpec extends Specification {

  "windowed with a list of length 5" should {
    "Return a windowed sequence of size 3" in {
      val ll = LazyList(1, 2, 3, 4, 5)
      ll.windowed(3).toList must be_==(List(Seq(1, 2, 3), Seq(2, 3, 4), Seq(3, 4, 5)))
    }

    "Return a windowed sequence of size 2" in {
      val ll = LazyList(1, 2, 3, 4, 5)
      ll.windowed(2).toList must be_==(List(Seq(1, 2), Seq(2, 3), Seq(3, 4), Seq(4, 5)))
    }

    "Return a singleton sequence of size 5" in {
      val ll = LazyList(1, 2, 3, 4, 5)
      ll.windowed(5).toList must be_==(List(Seq(1, 2, 3, 4, 5)))
    }

    "Return an empty windowed of size 6" in {
      val ll = LazyList(1, 2, 3, 4, 5)
      ll.windowed(6).toList must be_==(List.empty)
    }

  }

}
