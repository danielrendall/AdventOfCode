package uk.co.danielrendall.adventofcode.utils

/**
 * A counting map keeps track of the number of occurrences of objects of type T, using any form of number N for which
 * there exists a Numeric[N] (i.e. you can count in Ints, Longs, BigInts...)
 *
 * This is immutable; all operations return a new CountingMap
 *
 * @param map The initial map (see apply() in companion object to create an empty one)
 * @param num The Numeric instance for the type of number you want to use
 * @tparam T The type of things you're counting
 * @tparam N The type of number to use
 */
case class CountingMap[T, N](map: Map[T, N])
                            (using num: Numeric[N]) {

  def sortedSizeFirst: Seq[(T, N)] = map.toSeq.sortBy(_._2)(Ordering[N].reverse)

  /**
   * Add a single T to the map, returning a new map.
   * @param t The T to be added
   * @return New map with updated counts
   */
  def add(t: T): CountingMap[T, N] = add(t, num.one)

  /**
   * Add a specific number of Ts to the map
   * @param t The T to be added
   * @param howMany How many of the Ts to be added to the map
   * @return New map with updated counts
   */
  def add(t: T, howMany: N): CountingMap[T, N] = map.get(t) match {
    case Some(count) => CountingMap(map.+(t -> num.plus(count, howMany)))
    case None => CountingMap(map.+(t -> howMany))
  }

  /**
   * Add a sequence of Ts to the map
   *
   * @param iterable The Ts to be added
   * @return New map with updated counts
   */
  def add(iterable: Iterable[T]): CountingMap[T, N] =
    iterable.foldLeft(this) { case (cMap, item) => cMap.add(item)}

  /**
   * Add all the counts from another map of the same type
   *
   * @param other Another map of the same type
   * @return New map with updated counts
   */
  def addAll(other: CountingMap[T, N]): CountingMap[T, N] =
    other.map.foldLeft(this) { case (curMap, newEntry) => curMap.add(newEntry._1, newEntry._2) }
}

object CountingMap {

  /**
   * Create an empty counting map for types T and N
   */
  def apply[T, N]()
                 (using num: Numeric[N]): CountingMap[T, N] = new CountingMap(Map.empty)
}
