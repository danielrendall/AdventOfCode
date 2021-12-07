package uk.co.danielrendall.adventofcode.utils

import scala.collection.mutable

/**
 * A cache for computations of one value
 */
class ComputingMap[K, V](fn: K => V) {

  private val cache = new mutable.HashMap[K, V]()

  def get(k: K): V = cache.getOrElseUpdate(k, fn(k))

}
