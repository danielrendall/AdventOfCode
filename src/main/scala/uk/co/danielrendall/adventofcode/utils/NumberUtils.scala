package uk.co.danielrendall.adventofcode.utils

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

/**
 * Written for work, but I figure that since I wrote them I can use them...
 */
object NumberUtils {


  final val primes: Seq[Int] = {
    // We don't bother checking multiples of 2, 3 or 5
    val jumps = Array(4, 2, 4, 2, 4, 6, 2, 6, 4, 2, 4, 2, 4, 6, 2, 6)
    2 #:: 3 #:: 5 #:: LazyList.unfold((7, 0)) { case (candidate, jumpIndex) =>
      var toTest = candidate
      var index = jumpIndex
      while (!isPrime(toTest) && toTest < Int.MaxValue) {
        toTest = toTest + jumps(index)
        index = if (index == 15) 0 else index + 1
      }
      if (toTest == Int.MaxValue) None else {
        val next = toTest + jumps(index)
        index = if (index == 15) 0 else index + 1
        Some(toTest, (next, index))
      }
    }
  }

  final def isPrime(int: Int): Boolean = {
    if (Math.abs(int) > 1) {
      primeFactors(int).size == 1
      // 0 is not prime
    } else false
  }

  final def primeFactors(int: Int): Seq[Int] = {
    if (int > 0) {
      primeFactorsFor(int, sqrtCeiling(int), primes, List.empty)
    } else if (int == 0) {
      Seq(0)
    } else {
      primeFactorsFor(-int, sqrtCeiling(-int), primes, List(-1))
    }
  }

  private final def sqrtCeiling(int: Int): Int = Math.sqrt(int.toDouble).ceil.toInt


  @tailrec
  private final def primeFactorsFor(number: Int,
                                    sqrt: Int,
                                    primesToCheck: Seq[Int],
                                    accum: List[Int]): Seq[Int] = {
    if (number == 1) accum
    else {
      val next = primesToCheck.head
      if (next > sqrt) {
        number :: accum
      } else if (number % next == 0) {
        val nextNumber = number / next
        primeFactorsFor(nextNumber, sqrtCeiling(nextNumber), primesToCheck, next :: accum)
      } else {
        primeFactorsFor(number, sqrt, primesToCheck.tail, accum)
      }
    }

  }

  case class PrimeFactorSet(counts: SortedMap[Int, Int]) {
    def asBigInt: BigInt = {
      def mult(factors: List[(Int, Int)], accum: BigInt): BigInt = {
        factors match
          case head :: rest =>
            val newAccum = accum * head._1
            if (head._2 == 1) {
              mult(rest, newAccum)
            } else {
              mult((head._1 -> (head._2 - 1) :: rest) , newAccum)
            }

          case Nil =>
            accum
      }
      mult(counts.toList, BigInt(1))
    }

    def lcm(other: PrimeFactorSet): PrimeFactorSet = {
      val allPrimeFactors = counts.keySet ++ other.counts.keySet
      val map: SortedMap[Int, Int] = allPrimeFactors.map { pf =>
        pf -> Math.max(counts.getOrElse(pf, 0), other.counts.getOrElse(pf, 0))
      }.to(SortedMap)
      PrimeFactorSet(map)
    }
  }

  object PrimeFactorSet {

    def apply(int: Int): PrimeFactorSet = {
      PrimeFactorSet(primeFactors(int).groupBy(identity).view.mapValues(_.size).to(SortedMap))
    }

  }

}
