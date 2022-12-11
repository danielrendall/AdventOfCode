package uk.co.danielrendall.adventofcode.utils

import org.specs2.mutable.Specification

class LifoQueueSpec extends Specification {

  "LifoQueue" should {
    "Store a single element" in {
      val queue = new LifoQueue[Int]()
      queue.push(42)
      queue.pop() === 42
    }

    "Store two elements" in {
      val queue = new LifoQueue[Int]()
      queue.push(1)
      queue.push(2)
      queue.pop() === 1
      queue.pop() === 2
    }

    "Store two elements interleaved" in {
      val queue = new LifoQueue[Int]()
      queue.push(1)
      queue.pop() === 1
      queue.push(2)
      queue.pop() === 2
    }

    "Store ten elements" in {
      val queue = new LifoQueue[Int]()
      queue.push(1)
      queue.push(2)
      queue.push(3)
      queue.push(4)
      queue.push(5)
      queue.push(6)
      queue.push(7)
      queue.push(8)
      queue.push(9)
      queue.push(10)
      queue.pop() === 1
      queue.pop() === 2
      queue.pop() === 3
      queue.pop() === 4
      queue.pop() === 5
      queue.pop() === 6
      queue.pop() === 7
      queue.pop() === 8
      queue.pop() === 9
      queue.pop() === 10
    }

    "Store twenty elements interleaved" in {
      val queue = new LifoQueue[Int]()
      queue.push(1)
      queue.push(2)
      queue.pop() === 1
      queue.pop() === 2
      queue.push(3)
      queue.push(4)
      queue.push(5)
      queue.push(6)
      queue.pop() === 3
      queue.pop() === 4
      queue.pop() === 5
      queue.pop() === 6
      queue.push(7)
      queue.push(8)
      queue.push(9)
      queue.push(10)
      queue.push(11)
      queue.push(12)
      queue.pop() === 7
      queue.pop() === 8
      queue.pop() === 9
      queue.pop() === 10
      queue.pop() === 11
      queue.pop() === 12
      queue.push(13)
      queue.push(14)
      queue.push(15)
      queue.push(16)
      queue.push(17)
      queue.push(18)
      queue.push(19)
      queue.push(20)
      queue.pop() === 13
      queue.pop() === 14
      queue.pop() === 15
      queue.pop() === 16
      queue.pop() === 17
      queue.pop() === 18
      queue.pop() === 19
      queue.pop() === 20
    }

    "Push lots of elements at once" in {
      val queue = new LifoQueue[Int]()
      queue.pushAll(1 to 1000)
      queue.pop() === 1
      queue.pop() === 2
      (0 until 997).foreach(_ => queue.pop())
      queue.pop() === 1000
    }

    "Throw exception if there's nothing left" in {
      val queue = new LifoQueue[Int]()
      queue.pushAll(1 to 1000)
      (0 until 999).foreach(_ => queue.pop())
      queue.pop() === 1000
      queue.pop() must throwA[NoSuchElementException]
    }
  }

}
